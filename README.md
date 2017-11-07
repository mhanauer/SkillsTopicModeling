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
both$RB = as.character(both$RB)
head(both)
```
I think I need to add an id value.  Then I need to break the document down by word count.
```{r}
head(both)
bothCount = both %>%
  unnest_tokens(word, RB) %>%
  group_by(id) %>%
  summarize(words = n())
head(bothCount)
dim(both)
dim(bothCount)
bothCount$id
both$id
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
?count
```


Here is an example with AP and see how you can mirror this for your data
```{r}

data("AssociatedPress")
AssociatedPress
terms = Terms(AssociatedPress)
head(terms, 50)
# I think i is the document id v is the count and j must be the term?
# This converts non-tidy to tidy
ap_td = tidy(AssociatedPress); head(ap_td, 30)
# This converts tidy to document matrix term

ap_tdDTM = ap_td %>%
  cast_dtm(document, term, count)
# Document number, term identification, count for term
```

