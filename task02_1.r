setwd("D:\\faks\\is\\assignment1")
library(caret)
library(randomForest)

set.seed(1)

# The data has 1073 features and 77 entries
dataset = read.table("DLBCL.csv", header = TRUE, sep = ",")
dataset$class <- as.factor(dataset$class)
dataset$X <- NULL


fitness_function <- function(numberOfFeatures, accuracy, alpha) {
  # numberOfFeatures, accuracy are floats between 0 and 1
  alpha * numberOfFeatures + (1 - alpha) * accuracy
}

# Three fold cross validation
control <- trainControl(method = "cv", number = 3)
model = train(class ~ ., data = dataset, method = "knn", trControl = control)

# importance <- varImp(model, scale=FALSE)
# plot(importance)


rcontrol <- rfeControl(
  functions=rfFuncs,
  method="cv",
  number=3
)

results <- rfe(
  # Everything from dataset except the last column
  dataset[,1:length(dataset) - 1],
  dataset$class,
  rfeControl = rcontrol,
  #sizes = 2:16
)
print(results)


# super_model = train(
#   class ~ atU16812_s_at + atM16336_s_at + atHG1996.HT2044_at + atU01337_at,
#   data = dataset,
#   method = "knn",
#   trControl = control
# )
