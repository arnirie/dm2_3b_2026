#randomforest
library(palmerpenguins)
library(randomForest)
library(rpart)
library(adabag)

#clean data (take important features)
summary(penguins)
penguins_clean <- na.omit(penguins[,c("species",
                                      "bill_length_mm",
                                      "bill_depth_mm",
                                      "flipper_length_mm",
                                      "body_mass_g")])
table(penguins_clean$species)

#split 70/30; train/test
train_ids <- sample(1:nrow(penguins_clean), nrow(penguins_clean) * 0.7)
train_set <- penguins_clean[train_ids, ]
test_set <- penguins_clean[-train_ids, ]

#randomforest
rf_model <- randomForest(species ~ .,
             data = train_set,
             ntree = 500,
             mtry = 2) #sqrt(p)
rf_model

#generate prediction
rf_prediction <- predict(rf_model, test_set)
conf_rf <- table(rf_prediction,test_set$species)
#create conf matrix; compute the accuracy
accuracy_rf <- sum(diag(conf_rf)) / sum(conf_rf)
cat('Random Forest Accuracy: ', round(accuracy_rf * 100, 1), '%\n')

#single decision tree
tree_model <- rpart(species ~ .,
                    data = train_set,
                    method = "class"
)
dt_prediction <- predict(tree_model, 
                       test_set, 
                       type = "class")
conf_dt <-table(dt_prediction, test_set$species)
accuracy_dt <- sum(diag(conf_dt)) / sum(conf_dt)
cat('Single Decision Tree Accuracy: ', round(accuracy_dt * 100, 1), '%\n')


#boosting
boosting_model <- boosting(species ~ .,
         data = train_set,
         mfinal = 100)
boosting_prediction <- predict(boosting_model, test_set)
accuracy_boosting <- sum(diag(boosting_prediction$confusion)) / sum(boosting_prediction$confusion)
cat('Boosting: ', round(accuracy_boosting * 100, 1), '%\n')
