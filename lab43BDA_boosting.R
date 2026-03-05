library(palmerpenguins)
library(xgboost)

#clean_data

penguins_clean <- na.omit(penguins[,c("species",
                                      "bill_length_mm",
                                      "bill_depth_mm",
                                      "flipper_length_mm",
                                      "body_mass_g")])
summary(penguins_clean)

#split
train_ids <- sample(1:nrow(penguins_clean), nrow(penguins_clean) * 0.7)
train_set <- penguins_clean[train_ids, ]
test_set <- penguins_clean[-train_ids, ]

#specify matrix (as data : features) and label (target)
train_matrix <- as.matrix(train_set[, -1])
test_matrix <- as.matrix(test_set[, -1])
train_label <- as.numeric(train_set$species) - 1 #to make it 0
test_label <- as.numeric(test_set$species) - 1

#1 Preparation: DMatrix
dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dtest <- xgb.DMatrix(data = test_matrix, label = test_label)

xgb_model <- xgb.train(
  params = list(
    num_class = 3, 
    objective ="multi:softmax"
  ), 
  data = dtrain, 
  nrounds = 50
)

xgb_prediction <- predict(xgb_model, dtest)
table(xgb_prediction, test_label)
test_set$species
