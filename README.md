---
title: "Sentiment Analysis"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Getting the clean, but still need to get it ready for document type
```{r}
# library(topicmodels)
# library(modeltools)
# library(tm)
# library(methods)
# library(dplyr)
# library(stringr)
# library(dplyr)
# library(tidytext)
#setwd("~/Google Drive/MyProjects/Projects/NeedsAssessment/Data")
#rbbcsc = read.csv("RBBCSCStaffSurvey.csv", header = TRUE)
resources=rbbcsc$Q2
barriers = rbbcsc$Q3
rbbcscRB = data.frame(resources, barriers)
rbbcscRB = rbbcscRB[-c(1:2),]
head(rbbcscRB)

#mccsc = read.csv("mccscStaffSurvey.csv", header = TRUE)
resources=mccsc$Q2
barriers = mccsc$Q3
mccscRB = data.frame(resources, barriers)
mccscRB = mccscRB[-c(1:2),]
head(mccscRB)

both = rbind(rbbcscRB, mccscRB)
write.csv(both, "both.csv", row.names = FALSE)
both = read.csv("both.csv", header = TRUE, na.strings = c(""))
head(both)
both = na.omit(both)
dim(both)
resources = data.frame(both$resources)
barriers = data.frame(both$barriers)
both = data.frame(resources, barriers)
colnames(both) = c("resources", "barriers")
both =  reshape(both, varying = list(c("resources", "barriers")), times = c(1,2), direction = "long")
both = data.frame(both$resources)
n = dim(both)
n = n[1]
id = 1:n
both$id = id
colnames(both) = c("RB", "id")

head(both)
both = apply(both, 2, function(x){ifelse(x == "N/a", NA, ifelse(x == "N/A", NA, x))})
both = data.frame(na.omit(both))
both$RB = as.character(both$RB)
dim(both)

```
Ok no you have an ID word, and the count, so you should be able to turn this into a document term matrix.
```{r}
head(both)
bothCount = both %>%
  unnest_tokens(word, RB) %>%
  anti_join(stop_words) %>%
  group_by(id) %>%
  count(word)
colnames(bothCount) =c("document", "term", "count")
bothCount$document = as.integer(bothCount$document)
bothCount
bothCountDTM = bothCount %>%
  cast_dtm(document, term, count)
bothCountDTM
```
Now we can try and run the model
```{r}
bothLDA = LDA(bothCountDTM, k =4, control = list(seed=123))
bothLDATopics = tidy(bothLDA, matrix = "beta")
bothLDATopics

bothLDATop = bothLDATopics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
bothLDATop

bothLDATop %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
```

