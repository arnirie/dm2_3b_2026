library(palmerpenguins)
library(class)
library(e1071)
summary(penguins)

#KNN
#data cleaning
penguins_cleaned <- na.omit(penguins[ , c("species",
                      "bill_length_mm",
                      "bill_depth_mm",
                      "flipper_length_mm",
                      "body_mass_g")])

#split dataset into training & testing 
train_rows <- sample(1:nrow(penguins_cleaned), 0.7 * nrow(penguins_cleaned))
#training dataset
training_set <- penguins_cleaned[train_rows, ]
#testing dataset
testing_set <- penguins_cleaned[-train_rows, ]
summary(testing_set)

#data normalization
training_scaled <- scale(training_set[, -1])
testing_scaled <- scale(testing_set[, -1])
knn_prediction <- knn(train = training_scaled, 
    test = testing_scaled,
    cl = training_set$species)
#conf matrix
table(knn_prediction,testing_set$species)

#naive bayes
nb_model <- naiveBayes(species ~ ., data = training_set)
nb_prediction <- predict(nb_model, testing_set[,-1])
summary(nb_prediction)
summary(testing_set)
