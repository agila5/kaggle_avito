library(tidyverse)
library(lubridate)
library(tidytext)
library(tokenizers)
library(stopwords)
library(text2vec)
library(Matrix)
library(tictoc)



# carico il combi
load("feateng_combi")



### TF IDF features!!!


# pasting title and description into "text"
combi = combi %>%
  mutate(text = str_to_lower(paste(title, description, " ")))

# removing symbols and multiple spaces
combi = combi %>%
  mutate(text = gsub("[!?*#@\\\n-_%/()]", " ", text),
         text = gsub("\\s+", " ", text)) # multiple white spaces



# creating token
combi_token = combi %$% text %>%
  tokenize_word_stems(language = "russian") %>% 
  itoken()

# creating vocabulary
vocab = create_vocabulary(combi_token, 
                          ngram = c(1, 1), 
                          stopwords = stopwords("ru")) %>%
  prune_vocabulary(term_count_min = 3, 
                   doc_proportion_max = 0.4, 
                   vocab_term_max = 6500) %>% 
  vocab_vectorizer()

# tf idf features!
tfidf_temp = TfIdf$new(norm = "l2", sublinear_tf = T)

tfidf = create_dtm(combi_token, vocab) %>% 
  fit_transform(tfidf_temp)

rm(combi_token, vocab, tfidf_temp) #remove objects



# tolgo variabili problematiche (per ora)
combi = combi %>%
  select(-title, -description, -text)

# splitto train e test
testIds = which(is.na(combi[, "deal_probability"]))
notextTrain = combi[-testIds, ]
notextTest = combi[testIds, ]

# sparsification
yTrain = Matrix(as.matrix(notextTrain[, "deal_probability"]), sparse = T)
XTrain = sparse.model.matrix(deal_probability ~ 0 + ., notextTrain)

XTest = sparse.model.matrix(~ 0 + ., notextTest[, !colnames(notextTest) %in% "deal_probability"])

# adding tfidf features
XTrain = cbind(XTrain, tfidf[-testIds, ])
XTest = cbind(XTest, tfidf[testIds, ])



# salvo richi_train per usarlo su caret - attenti che è dgCMatrix!
save(yTrain , XTrain, file = "tfidf_train")
save(XTest, file = "tfidf_test")


