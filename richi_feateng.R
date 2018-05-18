library(tidyverse)
library(lubridate)
library(Matrix)
library(tictoc)



###
### AVITO DATA
###

# loading .Rdata
load("my_avito")

# merging train and test
combi = bind_rows(avito_train, avito_test)

# removing dots and commas and double spaces
combi = combi %>% 
  mutate(title = gsub("[.,]", " ", title),
         title = gsub("\\s+", " ", title),
         description = gsub("[.,]", " ", description),
         description = gsub("\\s+", " ", description))


#' dummy features:
#' presenza di descrizione
#' presenza di param1 - param3
#' presenza di immagine
#' trasformo price in log(1+price)

combi = combi %>%
  mutate(price = log1p(price),
         no_img = image %>% is.na() %>% as.integer(),
         no_descr = description %>% is.na() %>% as.integer(),
         no_p1 = param_1 %>% is.na() %>% as.integer(), 
         no_p2 = param_2 %>% is.na() %>% as.integer(), 
         no_p3 = param_3 %>% is.na() %>% as.integer()) %>%
  replace_na(list(image_top_1 = -1, # replace momentanei, copiati da kaggle
                  price = -1))


#' factor features

combi = combi %>%
  mutate(user_type = as.factor(user_type),
         region = as.factor(region),
         city = as.factor(city),
         parent_category_name = as.factor(parent_category_name),
         category_name = as.factor(category_name))


#' feature testuali per titolo e description:
#' _len = numero parole
#' _dig = numero digits
#' _cap = freq lettere maiuscole
#' _sym = freq simboli
#' _num = freq numeri
#' _lat = freq lettere latine - segnalano marche/specifiche

combi = combi %>%
  mutate(title_len = str_count(title, " "),
         title_dig = str_length(gsub(" ", "", title)),
         title_cap = str_count(title, "[A-Я]")/title_dig,
         title_sym = str_count(title, "[!?*#@\\-_%/()]")/title_dig,
         title_num = str_count(title, "[1-9]")/title_dig,
         title_lat = str_count(title, "[a-z]")/title_dig,
         descr_len = str_count(description, " "),
         descr_dig = str_length(gsub(" ", "", description)),
         descr_cap = str_count(description, "[A-Я]")/descr_dig,
         descr_sym = str_count(description, "[!?*#@\\-_%/()]")/descr_dig,
         descr_num = str_count(description, "[1-9]")/descr_dig,
         descr_lat = str_count(description, "[a-z]")/descr_dig) %>%
  replace_na(list(descr_len = 0,
                  descr_cap = 0,
                  descr_sym = 0,
                  descr_dig = 0,
                  descr_num = 0,
                  descr_lat = 0))


#' feature con date:
#' giorno della settimana
#' dummy weekend
#' basta così perché tutte le vendite sono a Marzo 2017
#' e train e test sono splittati temporalmente

combi = combi %>%
  mutate(act_weekday = as.factor(wday(activation_date)),
         act_weekend = as.integer(ifelse(act_weekday %in% c("6", "7"), 1, 0)))


#' trasformazione param_n in factor

combi = combi %>%
  mutate(param_1 = fct_lump(as.factor(param_1), n = 30),
         param_2 = fct_lump(as.factor(param_2), n = 30),
         param_3 = fct_lump(as.factor(param_3), n = 30)) %>%
  replace_na(list(param_1 = "Other",
                  param_2 = "Other",
                  param_3 = "Other"))







########################################################

# Pulizia e saving finale

# tolgo variabili problematiche (per ora)
combi = combi %>%
  select(-item_id, -user_id,
         -title, -description,
         -image, -activation_date)


# ri-splitto train e test
testIds = which(is.na(combi[, "deal_probability"]))
feateng_train = combi[-testIds, ]
feateng_test = combi[testIds, ]

# salvo richi_train per usarlo su caret
#save(feateng_train, file = "feateng_caret")


# metto in sparse
yTrain = Matrix(as.matrix(feateng_train[, "deal_probability"]), sparse = T)
XTrain = sparse.model.matrix(deal_probability ~ 0 + ., feateng_train)

XTest = sparse.model.matrix(~ 0 + ., feateng_test[, !colnames(feateng_test) %in% "deal_probability"])

# salvo
save(yTrain , XTrain, file = "sparse_train")
save(XTest, file = "sparse_test")


