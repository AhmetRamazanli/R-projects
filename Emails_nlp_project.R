library(data.table)
library(tidyverse)
library(text2vec)
library(caTools)
library(glmnet)
library(inspectdf)

# Importing emails dataset and getting familiarized with it.
path <- dirname(getSourceEditorContext()$path)
setwd(path)

df<-fread('emails.csv')

df %>% colnames()
df %>% glimpse()
df %>% inspect_na()

# Adding `id` column defined by number of rows
df <- df %>% add_column(id = 1:nrow(df)) %>% select(id,everything())

# Preparing data for fitting to the model
set.seed(123)
split<-df$spam %>% sample.split(SplitRatio = 0.8)
train <- df %>% subset(split == T)
test <- df %>% subset(split == F)

it_train<-train$text %>% 
  itoken(preprocessor=tolower,
         tokenizer=word_tokenizer,
         ids=train$id,
         progressbar=F)

vocab<-it_train %>% create_vocabulary()

vocab %>% arrange(desc(term_count)) %>% 
  head(100) %>% 
  tail(10)

vectorizer<-vocab %>% vocab_vectorizer()
dtm_train<-it_train %>% create_dtm(vectorizer)

dtm_train %>% dim()
identical(rownames(dtm_train),train$id)

# Using cv.glmnet for modeling
glmnet_classifier<-dtm_train %>% 
  cv.glmnet(y=train[['spam']],
            family='binomial',
            type.measure =  'auc',
            nfolds = 10,
            thresh=0.001,
            maxit=1000)


# Giving interpretation for train and test results

glmnet_classifier$cvm %>% max() %>% round(3) %>% paste("-> Max AUC") # train result
it_test <- test$text %>% tolower() %>% word_tokenizer()

it_test<-it_test %>% 
  itoken(ids=test$id, progressbar=F)

dtm_test<-it_test %>% create_dtm(vectorizer)

preds<-predict(glmnet_classifier,dtm_test,type='response')[,1]
glmnet:::auc(test$spam,preds) %>% round(2)
