##### Librerie ####
library(sparklyr)
library(tictoc)

#### Dati ####
load("my_avito")

#### Variabili ####
names(avito_train)

train <- avito_train
summary(train)
table(train$region)

#### sparklyr ####
sc <- spark_connect(master = "local", version = "2.3.0")
iris_tbl <- sdf_copy_to(sc, iris)
# Define the pipeline
labels <- c("setosa", "versicolor", "virginica")
pipeline <- ml_pipeline(sc) %>%
  ft_vector_assembler(
    c("Sepal_Width", "Sepal_Length", "Petal_Width", "Petal_Length"),
    "features"
  ) %>%
  ft_string_indexer_model("Species", "label", labels = labels) %>%
  ml_logistic_regression()

# Specify hyperparameter grid
grid <- list(
  logistic = list(
    elastic_net_param = c(0.25, 0.75),
    reg_param = c(1e-3, 1e-4)
  )
)

# Create the cross validator object
cv <- ml_cross_validator(
  sc, estimator = pipeline, estimator_param_maps = grid,
  evaluator = ml_multiclass_classification_evaluator(sc),
  num_folds = 3, parallelism = 4
)

# Train the models
cv_model <- ml_fit(cv, iris_tbl)
