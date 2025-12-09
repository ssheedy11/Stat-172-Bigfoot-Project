# Much of this code was followed from Dr. Lendie Follett's 
# "Reddit r/povertyfinance Analysis - A Case Study in Text Mining Methods"
# Thank you for allowing us to follow your documentation to help do LDA
# analysis for prediction on bigfoot sightings!

rm(list = ls())

library(ggplot2) #for quality visualizations
library(RColorBrewer) #for custom color palettes
library(dplyr) #for manipulating, aggregating, piping
library(tidytext) #for (tidy) text mining
library(topicmodels) #for LDA (topic modeling)
library(reshape2) #for reshaping data (long to wide or wide to long)


# RUN CODE STARTING HERE --------------------------------------------------

# Read in Data ------------------------------------------------------------

# manipulating text - use characters
source("src/Bigfoot Master Cleaning.R")

str(bigfoot_clean)

# Creating Tokenized Dataframe --------------------------------------------
# tokenize the observation text
tokens <- bigfoot_clean %>% unnest_tokens(word, observed)
#head(tokens)

#load stop_words data frame
data(stop_words)

#remove all rows consisting of a stop word
tokens_clean <- tokens %>% anti_join(stop_words)
#head(tokens_clean)


# Analyzing Word Frequencies ----------------------------------------------
# first, count the number of times each word shows up
tokens_count <- tokens_clean %>%
  #sorts from most frequent to least
  count(word, sort = TRUE) %>%
  #reorders the factor levels for the plot
  mutate(word = reorder(word,n))
#head(tokens_count)

#view the first 10 words:
ggplot(data = tokens_count[1:10,]) +
  geom_col(aes(x=word, y=n, fill = word),show.legend = FALSE, color = "black") +
  labs(x = "Word", y = "Count") +
  coord_flip() +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlGn"))(10)) +
  theme_bw()

ggsave("output/Word Frequencies.pdf")

### scale_fill_manual
# Ran into issue where color palette length was only 9, so I found documentation
# on how to manually extend the length with interpolation to use for our
# graphs
# https://stackoverflow.com/questions/16922988/interpolating-a-sequential-brewer-palette-as-a-legend-for-ggplot2
# https://novyden.blogspot.com/2013/09/how-to-expand-color-palette-with-ggplot.html
###


# Topic Modeling ----------------------------------------------------------
#Previously we counted the overall number of times each word showed up
#now we need to count the number of times each word shows up in each statement
#note that number is an identifier for each comment
tokens_count_lda <- tokens_clean %>%
  #include id so it counts within unique id
  count(number, word, sort = TRUE)%>%
  ungroup()

# Convert to Document Term Matrix for LDA
dtm <- tokens_count_lda %>%
  cast_dtm(number, word, n)

# Now we can perform LDA

# Setting Final LDA - 10 chosen topics (see below for testing)
ldafinal <- LDA(dtm, k = 12, control = list(seed = 1234))

# Collect beta matrix (a higher beta = word more important for topic)
topics_final <- tidy(ldafinal, matrix = "beta")

# Get a small data frame of the top 12 words for each topic
top_terms_final <- topics_final %>%
  group_by(topic) %>% #within each topic do some function
  top_n(12, beta) %>% #that function is take the top 12 in terms of beta
  ungroup() %>%
  arrange(topic, -beta) #order (just for the plot)

top_terms_final

top_terms_final %>%
  mutate(term = reorder(term, beta)) %>% #reorder term/word for plot
  ggplot() +
  geom_col(aes(x=term,y= beta, fill = factor(topic)),show.legend = FALSE, color = "black") + #like geom_bar but more flexible
  facet_wrap(~ topic, scales = "free") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlGn"))(12)) +
  theme_bw() +
  coord_flip() #make words easier to read

ggsave("output/Final LDA Groupings.pdf")

# LDA for prediction and modeling -----------------------------------------
documents <- tidy(ldafinal, matrix = "gamma")

documents_w <- documents %>%   
  select(document, topic, gamma) %>%
  dcast(document ~ topic, value.var = "gamma")

topic_names <- c("Heard Sounds", "Seen at Home", "On the Road", "Seen Tracks",
                 "Hunting", "Camping", "Animal Noises", "With Friends in Woods",
                 "Heard at Home", "Lake Trail", "Dogs", "Describing Creature")

colnames(documents_w) <- c("number", topic_names)


bigfoot_lda <- merge(documents_w, bigfoot_clean,  by="number", all = T)
#str(bigfoot_lda)

write.csv(bigfoot_lda, "output/bigfoot_clean_lda.csv", row.names = F)



# STOP HERE ---------------------------------------------------------------



# LDA Testing -------------------------------------------------------------
# Start with k = 10 (10 topics)
lda10 <- LDA(dtm, k = 10, control = list(seed = 1234))

# Collect beta matrix (a higher beta = word more important for topic)
topics10 <- tidy(lda10, matrix = "beta")

# Get a small data frame of the top 10 words for each topic
top_terms_10 <- topics10 %>%
  group_by(topic) %>% #within each topic do some function
  top_n(10, beta) %>% #that function is take the top 10 in terms of beta
  ungroup() %>%
  arrange(topic, -beta) #order (just for the plot)

top_terms_10

top_terms_10 %>%
  mutate(term = reorder(term, beta)) %>% #reorder term/word for plot
  ggplot() +
  geom_col(aes(x=term,y= beta, fill = factor(topic)),show.legend = FALSE, color = "black") + #like geom_bar but more flexible
  facet_wrap(~ topic, scales = "free") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlGn"))(10)) +
  theme_bw() +
  coord_flip() #make words easier to read

ggsave("output/LDA - 10 Topics.pdf")

# Looks pretty decent - I want to test to see if other numbers look better, 
# so let's drop down to 5

# Try with k = 5 (5 topics)
lda5 <- LDA(dtm, k = 5, control = list(seed = 1234))

# Collect beta matrix (a higher beta = word more important for topic)
topics5 <- tidy(lda5, matrix = "beta")

# Get a small data frame of the top 5 words for each topic
top_terms_5 <- topics5 %>%
  group_by(topic) %>% #within each topic do some function
  top_n(5, beta) %>% #that function is take the top 5 in terms of beta
  ungroup() %>%
  arrange(topic, -beta) #order (just for the plot)

top_terms_5

top_terms_5 %>%
  mutate(term = reorder(term, beta)) %>% #reorder term/word for plot
  ggplot() +
  geom_col(aes(x=term,y= beta, fill = factor(topic)),show.legend = FALSE, color = "black") + #like geom_bar but more flexible
  facet_wrap(~ topic, scales = "free") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlGn"))(5)) +
  theme_bw() +
  coord_flip() #make words easier to read

ggsave("output/LDA - 5 Topics.pdf")

# Does not look great - way too few categories. Let's see how adding one looks

# Try with k = 6 (6 topics)
lda6 <- LDA(dtm, k = 6, control = list(seed = 1234))

# Collect beta matrix (a higher beta = word more important for topic)
topics6 <- tidy(lda6, matrix = "beta")

# Get a small data frame of the top 8 words for each topic
top_terms_6 <- topics6 %>%
  group_by(topic) %>% #within each topic do some function
  top_n(6, beta) %>% #that function is take the top 8 in terms of beta
  ungroup() %>%
  arrange(topic, -beta) #order (just for the plot)

top_terms_6

top_terms_6 %>%
  mutate(term = reorder(term, beta)) %>% #reorder term/word for plot
  ggplot() +
  geom_col(aes(x=term,y= beta, fill = factor(topic)),show.legend = FALSE, color = "black") + #like geom_bar but more flexible
  facet_wrap(~ topic, scales = "free") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlGn"))(6)) +
  theme_bw() +
  coord_flip() #make words easier to read

ggsave("output/LDA - 6 Topics.pdf")

# Looks better than 5, but let's increase it to see if it gets even better
# (especially looking at the start from 10, I know we can get more topics)


# Try with k = 7 (7 topics)
lda7 <- LDA(dtm, k = 7, control = list(seed = 1234))

# Collect beta matrix (a higher beta = word more important for topic)
topics7 <- tidy(lda7, matrix = "beta")

# Get a small data frame of the top 8 words for each topic
top_terms_7 <- topics7 %>%
  group_by(topic) %>% #within each topic do some function
  top_n(7, beta) %>% #that function is take the top 8 in terms of beta
  ungroup() %>%
  arrange(topic, -beta) #order (just for the plot)

top_terms_7

top_terms_7 %>%
  mutate(term = reorder(term, beta)) %>% #reorder term/word for plot
  ggplot() +
  geom_col(aes(x=term,y= beta, fill = factor(topic)),show.legend = FALSE, color = "black") + #like geom_bar but more flexible
  facet_wrap(~ topic, scales = "free") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlGn"))(7)) +
  theme_bw() +
  coord_flip() #make words easier to read

ggsave("output/LDA - 7 Topics.pdf")

# Similar to 6 topics. There is additional topics that I know can be added, 
# but still an improvement from only 6 topics


# Try with k = 8 (8 topics)
lda8 <- LDA(dtm, k = 8, control = list(seed = 1234))

# Collect beta matrix (a higher beta = word more important for topic)
topics8 <- tidy(lda8, matrix = "beta")

# Get a small data frame of the top 8 words for each topic
top_terms_8 <- topics8 %>%
  group_by(topic) %>% #within each topic do some function
  top_n(8, beta) %>% #that function is take the top 8 in terms of beta
  ungroup() %>%
  arrange(topic, -beta) #order (just for the plot)

top_terms_8

top_terms_8 %>%
  mutate(term = reorder(term, beta)) %>% #reorder term/word for plot
  ggplot() +
  geom_col(aes(x=term,y= beta, fill = factor(topic)),show.legend = FALSE, color = "black") + #like geom_bar but more flexible
  facet_wrap(~ topic, scales = "free") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlGn"))(8)) +
  theme_bw() +
  coord_flip() #make words easier to read

ggsave("output/LDA - 8 Topics.pdf")

# Looking much better - I know that there are topics we can still add though,
# so 10 is looking like it is the best choice so far. 
# Let's see if 9 is any better

# Try with k = 9 (9 topics)
lda9 <- LDA(dtm, k = 9, control = list(seed = 1234))

# Collect beta matrix (a higher beta = word more important for topic)
topics9 <- tidy(lda9, matrix = "beta")

# Get a small data frame of the top 8 words for each topic
top_terms_9 <- topics9 %>%
  group_by(topic) %>% #within each topic do some function
  top_n(9, beta) %>% #that function is take the top 8 in terms of beta
  ungroup() %>%
  arrange(topic, -beta) #order (just for the plot)

top_terms_9

top_terms_9 %>%
  mutate(term = reorder(term, beta)) %>% #reorder term/word for plot
  ggplot() +
  geom_col(aes(x=term,y= beta, fill = factor(topic)),show.legend = FALSE, color = "black") + #like geom_bar but more flexible
  facet_wrap(~ topic, scales = "free") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlGn"))(9)) +
  theme_bw() +
  coord_flip() #make words easier to read

ggsave("output/LDA - 9 Topics.pdf")

# Looks good, but still missing some topics that I think can be added.
# Let's go to 11 for completeness to see if there are any topics missing.

# Try with k = 11 (11 topics)
lda11 <- LDA(dtm, k = 11, control = list(seed = 1234))

# Collect beta matrix (a higher beta = word more important for topic)
topics11 <- tidy(lda11, matrix = "beta")

# Get a small data frame of the top 8 words for each topic
top_terms_11 <- topics11 %>%
  group_by(topic) %>% #within each topic do some function
  top_n(11, beta) %>% #that function is take the top 8 in terms of beta
  ungroup() %>%
  arrange(topic, -beta) #order (just for the plot)

top_terms_11

top_terms_11 %>%
  mutate(term = reorder(term, beta)) %>% #reorder term/word for plot
  ggplot() +
  geom_col(aes(x=term,y= beta, fill = factor(topic)),show.legend = FALSE, color = "black") + #like geom_bar but more flexible
  facet_wrap(~ topic, scales = "free") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlGn"))(11)) +
  theme_bw() +
  coord_flip() #make words easier to read

ggsave("output/LDA - 11 Topics.pdf")

# 11th topic adds a topic focused around dogs - I want to see if we can add
# a topic that mentions a creature or describes something - let's move on
# to a 12th topic

# Try with k = 11 (11 topics)
lda12 <- LDA(dtm, k = 12, control = list(seed = 1234))

# Collect beta matrix (a higher beta = word more important for topic)
topics12 <- tidy(lda12, matrix = "beta")

# Get a small data frame of the top 8 words for each topic
top_terms_12 <- topics12 %>%
  group_by(topic) %>% #within each topic do some function
  top_n(12, beta) %>% #that function is take the top 8 in terms of beta
  ungroup() %>%
  arrange(topic, -beta) #order (just for the plot)

top_terms_12

top_terms_12 %>%
  mutate(term = reorder(term, beta)) %>% #reorder term/word for plot
  ggplot() +
  geom_col(aes(x=term,y= beta, fill = factor(topic)),show.legend = FALSE, color = "black") + #like geom_bar but more flexible
  facet_wrap(~ topic, scales = "free") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "YlGn"))(12)) +
  theme_bw() +
  coord_flip() #make words easier to read

ggsave("output/LDA - 12 Topics.pdf")

# 12th topic adds creature and it's description - this is the one!
# Let's stop here!

