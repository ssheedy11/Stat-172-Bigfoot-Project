rm(list= ls())
library(tidyverse)
library(pROC)
library(randomForest)
library(tidymodels)
library(rpart) #classification trees
library(rpart.plot) #make pretty trees
library(RColorBrewer)
library(caret)

#Read in the cleaned LDA data
bigfoot_RF <- read.csv("output/bigfoot_clean_lda.csv")

#Ensuring that observation is read in as a factor. Dropping observed as the LDA takes the place of that and number is an identifier.
bigfoot_RF <- bigfoot_RF %>% mutate(classification = as.factor(classification)) %>% select(-c(observed, number))

#Setting the seed number to 1234 to make this reproducible
RNGkind(sample.kind = "default")
set.seed(1234)


#Splitting the observations into 80% training and 20% data
train.idx <- sample(x=1:nrow(bigfoot_RF), size = .8*nrow(bigfoot_RF))
#creates the training data
train.df <- bigfoot_RF[train.idx,]

#creates the testing data
test.df <- bigfoot_RF[-train.idx,]


#Tree fitting

#Setting the seed number to 1234 to make this reproducible
set.seed(1234)
#Creates the classification tree using the training data.
ctree <- rpart(classification ~ ., data = train.df, method = 'class')

#look at the object tree
ctree
#plots out the tree
rpart.plot(ctree)


##Tuning our tee


#Look at the cp values and cross validation values to minimize sample error.
printcp(ctree)


#Setting the seed number to 1234 to make this reproducible
set.seed(1234)
#Creates a big tree that we will prune down later
ctree <- rpart(classification ~ ., 
               data = train.df, 
               method = 'class',
               control = rpart.control(cp = 0.001, minsplit = 1)) 

#Look at the cp values and cross validation values to minimize sample error.
printcp(ctree)
#prune it down!
#automatically/reproducibly grab the optimal cp number that corresponds to the smallest xerror
optcp <- ctree$cptable[which.min(ctree$cptable[,"xerror"]),"CP"]

#Prunes the tree to the value of the optimal cp.
ctree2 <- rpart::prune(ctree, cp = optcp)

#Plots the pruned tree for us to visually see.
rpart.plot(ctree2)


##Model validation + Prediction
#make column of predictions based on pruned tree
#do this in the test dataset so we can see how well we do on truly new data

test.df$classification_pred <- predict(ctree2, test.df, type = "class")
table(test.df$classification_pred, test.df$classification)

#Accuracy: (421+357)/997 = 78.03%
#Sensitivity: 421/(421+147) = 74.1% 
#Specificity: 357/(357+72) = 83.2%

#Setting the seed number to 1234 to make this reproducible
set.seed(1234)
#Creates a random forest saved as my forest using the mtry value
myforest <- randomForest(classification ~ .,
                         data = train.df,
                         ntree = 1000,
                         mtry = 5,
                         importance = TRUE) #so we can get importance plot

#Prints myforest out
myforest

#Accuracy: (1637+1465)/3986 = 77.8%
#Sensitivity: 1637/(1637+343) = 82.6%
#Specificity: 1465/(1465+541) = 73.0%

#Creates a classification random forest that has 1000 trees.
rf_model <- rand_forest(mtry = tune(),
                        trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("randomForest")

#Uses all the predictors to predict
rf_rec <-recipe(classification ~ ., data = train.df)

#combines the ranodom forest and recipe to train and tune together.
rf_wf <- workflow() %>%
  add_model(rf_model)%>%
  add_recipe(rf_rec)

#Setting the seed number to 1234 to make this reproducible
set.seed(1234)
folds <- vfold_cv(train.df, v= 5)

#Evaluates the random forest using 5 fold cross-validation measuring AUC
rf_tuned <- tune_grid(
  rf_wf,
  resamples = folds,
  grid = tibble(mtry = c(1:12)),
  metrics = metric_set(roc_auc)
)
#Summarizes and saves the values from rf_tuned.
rf_results <- rf_tuned %>% collect_metrics()

#Plots the AUC values against the m mtry values
ggplot(data = rf_results)+
  geom_line(aes(x = mtry, y = mean)) +
  geom_point(aes(x = mtry, y = mean))+
  labs(x = "m mtry value", y = "Area Under the Curve (AUC)")+
  theme_bw()+
  scale_x_continuous(breaks = c(1:12))
#ggsave("output/Random Forest AUC.pdf")

#We can see that 5 is the best value.

#Selects teh best paramters to optimize roc auc
best_params <- select_best(rf_tuned, metric = "roc_auc")

#Cretes a classification random forest of 1000 trees using the mtry and parameters defined in previous lines of code.
finalforest <- randomForest(classification ~ .,
                            data = train.df,
                            ntree = 1000,
                            mtry = best_params %>% pull(mtry),
                            importance = TRUE) #so we can get importance plot


###Results

#Goal is prediction
#assuming positive event is Class A!
pi_hat <- predict(finalforest, test.df, type = "prob")[, "Class A"] #Gets vector of Class A probability
forest_rocCurve <- roc(response = test.df$classification, 
                predictor = pi_hat, #probabilities of Class A, our positive event
                levels = c("Class B", "Class A")) #First negative event, then positive
pdf("output/ROC Curve RF.pdf")
plot(forest_rocCurve, print.thres = TRUE, print.auc = TRUE)
title("ROC Plot Random Forest", line = 3)
dev.off()



#If we set pi* = .459, we can achieve a specificity of .716 and a sensitivity of .838       pi* (specificity, sensitivity)

#We predict Class B 71.6% of the time when it is actually Class B. And we'll predict Class A 83.8% of the time when it is actually Class A.
#The area under the curve is .844

#makes a column of predicted values in our test data
pi_star <- coords(forest_rocCurve, "best", ret = "threshold")$threshold[1]
#makes a new column of predictions
test.df$forest_pred <- ifelse(pi_hat > pi_star, "1", "0") %>% as.factor()

#note in prediction we do not use code that looks like this
#$new_data$precitced_classes <- predict(finalforest, new_data, type = "class")

#######
#Interpretion:
######

varImpPlot(finalforest, type = 1)
#LDA fields seem to be the most important
#Describing creature is the most imporant, then on the road, and third most important is heard sounds.
vi <- as.data.frame(varImpPlot(finalforest, type = 1))
vi$Variable <- row.names(vi)
ggplot(data = vi) +
  geom_bar(aes(x = reorder(Variable, MeanDecreaseAccuracy), 
               weight = MeanDecreaseAccuracy,
               fill = MeanDecreaseAccuracy),
           position = "identity", color = "Black") +
  coord_flip() +
  scale_fill_gradientn(colors = colorRampPalette(brewer.pal(9, "YlGn"))(100)) + 
  labs(x = "Variable Name", y = "Importance", fill = "Importance", title = "Variable Importance") + 
  theme_bw()
ggsave("output/Variable Importance Plot.pdf")


#fitting a logistic regression using only the most important variables as explanatory variables

#first create a bernoulli RV
bigfoot_RF$classification<- ifelse(bigfoot_RF$classification == "Class A", 1, 0)

#Creates a logistic regression model with 1 predictor
m1 <- glm(classification ~ Describing.Creature, data = bigfoot_RF,
          family = binomial(link = "logit"))
#Collects the AIC for m1
AIC(m1)#6441.479

#Creates a logistic regression model with 2 predictors
m2 <- glm(classification ~ Describing.Creature + On.the.Road, data = bigfoot_RF,
          family = binomial(link = "logit"))

#Collects the AIC for m2
AIC(m2) #5656.683

#Creates a logistic regression model with 3 predictors
m3 <- glm(classification ~  Describing.Creature + On.the.Road + Seen.Tracks, data = bigfoot_RF,
          family = binomial(link = "logit"))

#Collects the AIC for m3
AIC(m3) #5633.872

#Creates a logistic regression model with 4 predictors
m4 <- glm(classification ~ Describing.Creature + On.the.Road + Seen.Tracks + Animal.Noises, data = bigfoot_RF,
          family = binomial(link = "logit"))
#Collects the AIC for m4
AIC(m4) #5554.593

#Creates a logistic regression model with 5 predictors
m5 <- glm(classification ~ Describing.Creature + On.the.Road + Seen.Tracks + Animal.Noises + With.Friends.in.Woods, data = bigfoot_RF,
          family = binomial(link = "logit"))
#Collects the AIC for m5
AIC(m5) #5347.454

#Creates a logistic regression model with 6 predictors
m6 <- glm(classification ~ Describing.Creature + On.the.Road + Seen.Tracks + Animal.Noises + With.Friends.in.Woods + Camping, data = bigfoot_RF,
          family = binomial(link = "logit"))
#Collects the AIC for m6
AIC(m6) #5343.846

#Creates a logistic regression model with 6 predictors
m7 <- glm(classification ~ Describing.Creature + On.the.Road + Seen.Tracks + Animal.Noises + With.Friends.in.Woods + Camping + Heard.at.Home, data = bigfoot_RF,
          family = binomial(link = "logit"))
#Collects the AIC for m6
AIC(m7) #5345.206


#m6 would be our final descriptive model that we will use to supplement the random forest. The plan is to use the random forest for predictions
#and m6 for descriptive statements

summary(m6)

coef(m6)
exp(coef(m6))


confint(m3)

#https://stackoverflow.com/questions/37897252/plot-confusion-matrix-in-r-using-ggplot

cm <- table(test.df$forest_pred, test.df$classification)

conf_df <- as.data.frame(cm)
names(conf_df) <- c("Predicted", "Actual", "Freq")

# Create the confusion matrix plot
ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), color = "black", size = 6) +
  scale_fill_gradientn(colors = colorRampPalette(brewer.pal(9, "YlGn"))(100)) +
  labs(
    title = "Confusion Matrix",
    x = "Actual Class",
    y = "Predicted Class"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none"
  ) +
  scale_x_discrete(labels = c("Class A", "Class B")) +
  scale_y_discrete(labels = c("Class B", "Class A"))
  coord_fixed()  # Makes the tiles square


ggsave("output/Random Forest Confusion Matrix.pdf")





