#### Libraries
library(readr)


### Loading data from csv.zip
avito_train = read_csv("train.csv.zip")


### Saving as .Rdata
save(avito_train, file = "my_avito")