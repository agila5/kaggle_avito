### Librerie
library(tidyverse)
library(tidytext)
library(stopwords)
library(topicmodels)

### Data
load("my_avito")

### Combine data
traintest <- bind_rows(avito_train, avito_test)

### Remove train e test (tanto li posso ricaricare)
rm(avito_train)
rm(avito_test)
gc()

### Stopwords dictionary
my_stopwords <- bind_rows(
  as.tibble(stopwords("ru")) %>% rename(word = value) %>% mutate(lexicon = "SNOWBALL"),
  as.tibble(stopwords("ru", source = "stopwords-iso")) %>% 
              rename(word = value) %>% 
              mutate(lexicon = "SNOWBALL"))
)

### Split the dataset in 10 parts
# Of course it would be better if you don't divide the data
# Un'idea alternativa potrebbe usare sto sparklyr ma non ho tempo di leggermi
# come funziona la sintassi ora :(

traintest <- traintest %>%
  mutate(my_split = sample(1:10, nrow(traintest), replace = TRUE))

### My tidysext pt1
my_tidytext <- traintest %>%
  filter(my_split == 1) %>%
  group_by(item_id) %>%
  select(item_id, description) %>%
  unnest_tokens(word, description) %>%
  count(word) %>%
  ungroup()

rm(my_tidytext)
gc()

### My document-term matrix
my_dtm <- my_tidytext %>%
  cast_dtm(item_id, word, n)

### My LDA
#Il parametro k (cioè il numero di topic nei vari argomenti necessita di tuning)
# Il numero k = 20 l'ho scelto a caso
my_LDA <- LDA(my_dtm, k = 20)
my_tidyLDA <- tidy(my_LDA, matrix = "gamma")


### Result
my_tidyLDA %>%
  arrange(document) %>%
  spread(key = "topic", value = "gamma")
