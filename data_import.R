#### Libraries
library(readr)


### Loading data from csv.zip
avito_train = read_csv("train.csv.zip")
avito_test = read_csv("test.csv.zip")

periods_train = read_csv("periods_train.csv.zip")
periods_test = read_csv("periods_test.csv.zip")


### Saving as .Rdata
save(avito_train, avito_test, file = "my_avito")
save(periods_train, periods_test, file = "my_periods")
