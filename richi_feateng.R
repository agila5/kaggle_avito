library(tidyverse)
library(lubridate)
library(tidytext)
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

combi = combi %>%
  mutate(no_img = image %>% is.na() %>% as.integer(),
         no_descr = description %>% is.na() %>% as.integer(),
         no_p1 = param_1 %>% is.na() %>% as.integer(), 
         no_p2 = param_2 %>% is.na() %>% as.integer(), 
         no_p3 = param_3 %>% is.na() %>% as.integer()) %>%
  replace_na(list(image_top_1 = -1, 
                  price = -1))


#' factor features

combi = combi %>%
  mutate(user_type = user_type %>% as.factor(),
         region = region %>% as.factor(),
         city = city %>% as.factor(),
         parent_category_name = parent_category_name %>% as.factor(),
         category_name = category_name %>% as.factor())


#' feature testuali per titolo e description:
#' count di lettere maiuscole
#' count di simboli
#' count di digits
#' lunghezza
#' count di numeri
#' count di lettere latine - di solito sono specifiche o marche

combi = combi %>%
  mutate(title_len = str_length(title),
         title_cap = str_count(title, "[A-Я]"),
         title_sym = str_count(title, "[!?*#@\\\n-_%/()]"),
         title_dig = nchar(gsub(" ", "", title)),
         title_num = str_count(title, "[1-9]"),
         title_lat = str_count(title, "[a-z]"),
         descr_len = str_length(description),
         descr_cap = str_count(description, "[A-Я]"),
         descr_sym = str_count(description, "[!?*#@\\\n-_%/()]"),
         descr_dig = nchar(gsub(" ", "", description)),
         descr_num = str_count(description, "[1-9]"),
         descr_lat = str_count(description, "[a-z]")) %>%
  replace_na(list(descr_len = 0,
                  descr_cap = 0,
                  descr_sym = 0,
                  descr_dig = 0,
                  descr_num = 0,
                  descr_lat = 0))


#' feature con date
#' giorno del mese
#' giorno della settimana
#' mese dell'anno
#' no settimana e anno che è tutto Marzo-Aprile 2017

combi = combi %>%
  mutate(act_mday = mday(activation_date),
         act_wday = wday(activation_date),
         act_month = month(activation_date))


# Pulizia e saving finale

# tolgo variabili problematiche (per ora)
combi = combi %>%
  select(-item_id, -user_id,
         -title, -description,
         -param_1, -param_2, -param_3,
         -image, -activation_date)


# ri-splitto train e test
testIds = which(is.na(combi[, "deal_probability"]))
richi_train_due = combi[-testIds, ]
richi_test_due = combi[testIds, ]


# salvo richi_train per usarlo su caret
save(richi_train_due, file = "feateng_train")

