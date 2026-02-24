library(palmerpenguins)
library(rpart)
#omit NA
penguins <- na.omit(penguins)

#View(penguins)
summary(penguins)
plot(penguins$bill_length_mm, 
     penguins$bill_depth_mm, 
     col = as.numeric(penguins$species),
     pch = 19,
     cex = 0.75,
     xlab = "Bill Length (mm)",
     ylab = "Bill Depth (mm)",
     main = "Bill Length and Depth Plot per Specie"
     )
legend("topright",
      legend = levels(penguins$species),
      col = 1:3,
      pch = 19
       )

#create a decision tree model
tree_model <- rpart(species ~ bill_length_mm + bill_depth_mm,
      data = penguins,
      method = "class"
      )

plot(tree_model, 
     margin = 0.25
     )
text(tree_model)

#predict
predictions <- predict(tree_model, 
        penguins, 
        type = "class")
confusion_matrix <- table(penguins$species, predictions)
confusion_matrix
total <- sum(confusion_matrix)
total_correct <- sum(diag(confusion_matrix))
accuracy <- total_correct / total
cat("Model's accuracy: ", round(accuracy * 100, 2), "%")
