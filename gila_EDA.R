### Libraries
library(tidyverse)
library(tictoc)

### Loading dataset
tic()
load("my_avito")
toc()

### Roba per minniti
#img_0 <- read_csv("0_imgages.csv")


#avito_train %>%
#  filter(image %in% img_0$`0`)

#per_minniti <- avito_train %>%
#  filter(is.element(paste0(avito_train$image, ".jpg"), img_0$`0`)) %>%
#  select(image, deal_probability) %>%
#  mutate(is_zero = if_else(deal_probability == 0, 0, 1))

#write_csv(per_minniti, "file_per_minniti.csv")
