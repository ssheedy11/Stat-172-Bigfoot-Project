rm(list = ls())

library(pROC)
library(glmnet) #for fitting lasso, ridge regressions (GLMs)
library(tidyverse)

bigfoot_lda <- read.csv("output/bigfoot_clean_lda.csv")

bigfoot_rl <- bigfoot_lda %>%
  mutate(classification = ifelse(classification == "Class A",1,0)) %>%
  select(-c(observed,number))

RNGkind(sample.kind = "default")
set.seed(23591)
train.idx <- sample(x = 1:nrow(bigfoot_rl), size = .7*nrow(bigfoot_rl))
train.df <- bigfoot_rl[train.idx,]
test.df <- bigfoot_rl[-train.idx,]

#start with a traditional logistic regression model fit with MLE
lr_mle <- glm(classification ~.,
              data = train.df,
              family = binomial(link = "logit"))

lr_ml_coefs <- coef(lr_mle)

#build X matrices for lasso, ridge regression that "one-hot" codes any factors that are present
x.train <- model.matrix(classification ~., data = train.df)[,-1]
x.test <- model.matrix(classification ~., data = test.df)[,-1]

#also need "vectorized" y vectors
y.train <- as.vector(train.df$classification)
y.test <- as.vector(test.df$classification)

#use cross validation to quickly fit and assess many lasso and ridge regression models
lr_lasso_cv <- cv.glmnet(x.train, y.train, family = binomial(link = "logit"), alpha=1)
lr_ridge_cv <- cv.glmnet(x.train, y.train, family = binomial(link = "logit"), alpha=0)

plot(lr_lasso_cv, sign.lambda = 1)
plot(lr_ridge_cv, sign.lambda = 1)

best_lasso_lambda <- lr_lasso_cv$lambda.min #this is the lambda that minimizes out of sample error (based on cross validation)
best_ridge_lambda <- lr_ridge_cv$lambda.min

#we can also look at the coefficients from the best models
lr_lasso_coefs <- coef(lr_lasso_cv, s = best_lasso_lambda)
lr_ridge_coefs <- coef(lr_ridge_cv, s = best_ridge_lambda)

ggplot() +
  geom_point(aes(as.vector(lr_ml_coefs), as.vector(lr_ridge_coefs))) +
  geom_abline(aes(slope = 1, intercept = 0)) +
  xlim(c(-10,10)) + ylim(c(-10,10))

ggplot() +
  geom_point(aes(as.vector(lr_ml_coefs), as.vector(lr_lasso_coefs))) +
  geom_abline(aes(slope = 1, intercept = 0)) +
  xlim(c(-10,10)) + ylim(c(-10,10))

final_lasso <- glmnet(x.train, y.train, family = binomial(link = "logit"), alpha = 1,
                      lambda = best_lasso_lambda)

final_ridge <- glmnet(x.train, y.train, family = binomial(link = "logit"), alpha = 0,
                      lambda = best_ridge_lambda)

test.df.preds <- test.df %>% 
  mutate(mle_pred = predict(lr_mle, test.df, type="response"),
         lasso_pred = predict(final_lasso, x.test, type = "response")[,1],
         ridge_pred = predict(final_ridge, x.test, type = "response")[,1])

cor(test.df.preds$mle_pred, test.df.preds$lasso_pred)
#the lasso predictions are 99.37406% correlated with the mle predictions
cor(test.df.preds$mle_pred, test.df.preds$ridge_pred)
#the ridge predictions are 99.6455% correlated with the mle predictions

mle_rocCurve <- roc(response = as.factor(test.df.preds$classification),
                    predictor = test.df.preds$mle_pred,
                    levels = c("0","1"))
pdf("output/ROC Curve MLE.pdf")
plot(mle_rocCurve, print.thres = TRUE, print.auc = TRUE)
title("ROC Plot MLE", line = 3)
dev.off()


lasso_rocCurve <- roc(response = as.factor(test.df.preds$classification),
                      predictor = test.df.preds$lasso_pred,
                      levels = c("0","1"))
pdf("output/ROC Curve Lasso.pdf")
plot(lasso_rocCurve, print.thres = TRUE, print.auc = TRUE)
title("ROC Plot Lasso", line = 3)
dev.off()

ridge_rocCurve <- roc(response = as.factor(test.df.preds$classification),
                      predictor = test.df.preds$ridge_pred,
                      levels = c("0","1"))
pdf("output/ROC Curve Ridge.pdf")
plot(ridge_rocCurve, print.thres = TRUE, print.auc = TRUE)
title("ROC Plot Ridge", line = 3)
dev.off()

#make data frame of MLE ROC info
mle_data <- data.frame(
  Model = "MLE",
  Specificity = mle_rocCurve$specificities,
  Sensitivity = mle_rocCurve$sensitivities,
  AUC = as.numeric(mle_rocCurve$auc)
)
#make data frame of lasso ROC info
lasso_data <- data.frame(
  Model = "Lasso",
  Specificity = lasso_rocCurve$specificities,
  Sensitivity = lasso_rocCurve$sensitivities,
  AUC = lasso_rocCurve$auc %>% as.numeric
)
#make data frame of ridge ROC info
ridge_data <- data.frame(
  Model = "Ridge",
  Specificity = ridge_rocCurve$specificities,
  Sensitivity = ridge_rocCurve$sensitivities,
  AUC = ridge_rocCurve$auc%>% as.numeric
)

# Combine all the data frames
roc_data <- rbind(mle_data, lasso_data, ridge_data)

# Plot the data
ggplot() +
  geom_line(aes(x = 1 - Specificity, y = Sensitivity, color = Model),data = roc_data) +
  geom_text(data = roc_data %>% group_by(Model) %>% slice(1), 
            aes(x = 0.75, y = c(0.75, 0.65, 0.55), colour = Model,
                label = paste0(Model, " AUC = ", round(AUC, 3)))) +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "1 - Specificity", y = "Sensitivity", color = "Model") +
  theme_minimal()

ggsave("output/ROC Curve Comparison Regression Only.pdf")

################
#Lasso Analysis#
################

# turn into a data frame so it's readable
lasso_df <- data.frame(
  feature = rownames(lr_lasso_coefs),
  coef = as.numeric(lr_lasso_coefs)
)

#what columns did lasso drop
dropped <- lasso_df %>% filter(coef == 0) %>% pull(feature)
dropped

# which ones actually stayed?
kept <- lasso_df %>% filter(coef != 0) %>% pull(feature)
kept

#########
#WEATHER#
#########
#not important:
#   -temperature(high,mid,low)
#   -humidity
#   -dew point
#   -precip intensity
#   -uv index
#   -visibility

#important:
#   -cloud cover
#   -precip probability
#   -precip type
#   -pressure
#   -wind bearing
#   -wind speed

#######
#OTHER#
#######
#not important:
#   -latitude (north vs south)

#important:
#   -longitude(east vs west)
#   -moon phase
#   -unknown date and month

############
#LDA TOPICS#
############
#not important:
#   -Hunting

#important:
#   -every other topic

ridge_coef_df <- as.matrix(lr_ridge_coefs) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("term") %>%
  rename(coef = 's=0.017792') %>%
  arrange(coef) %>%                    # sorted while numeric
  mutate(coef = format(coef, scientific = FALSE))  # printed nicely
ridge_coef_df

lasso_coef_df <- as.matrix(lr_lasso_coefs) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("term") %>%
  rename(coef = 's=0.004725711') %>%
  arrange(coef) %>%                    # sorted while numeric
  mutate(coef = format(coef, scientific = FALSE))  # printed nicely
lasso_coef_df

#plot both the ridge and lasso coefficients(i.e. x is ridge coef and y is lasso coef)

coef_df <- merge(
  ridge_coef_df %>% rename(ridge = coef),
  lasso_coef_df %>% rename(lasso = coef),
  by = "term",
  all = TRUE
)

ggplot(coef_df) +
  geom_point(aes(x = as.numeric(ridge),
                 y = as.numeric(lasso))) +
  geom_abline(intercept = 0, slope = 1, color = "red")