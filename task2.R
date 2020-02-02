setwd("D:\\faks\\is\\assignment1\\")
library(caret)
library(GA)
library(parallel)
library(doParallel)

set.seed(1)

# The data has 1073 features and 77 entries
dataset = read.table("DLBCL.csv", header = TRUE, sep = ",")
#dataset$class <- as.factor(dataset$class)
dataset$X <- NULL

# Select features


# Three fold cross validation
control <- trainControl(
  method = "cv",
  number = 3,
)

fitness_function <- function (x) {
  x <- !!x
  x[length(dataset)] <- T
  
  curr_dataset <- dataset[,x]
  
  
  # model <- train(
  #   class ~ .,
  #   data = curr_dataset,
  #   method = "knn",
  #   trControl = control
  # )
  # accuracy <- max(model$results[,2])
  
  features <- sum(x) / length(x)
  features <- 1 - features
  features
}

# TODO zacetne resitve so samo nicle
GA <- ga(
  type = "binary",
  nBits = length(dataset) - 1,
  maxiter = 1000,
  parallel = T,
  run = 200,
  popSize = 50,
  pcrossover = 0.5,
  pmutation = 0.5,
  fitness = fitness_function
)




