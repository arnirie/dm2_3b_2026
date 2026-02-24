#CLUSTERING
library(palmerpenguins)

penguins_cleaned <- na.omit(penguins)

#unlabeled data; get only lengh & depth of bill
penguins_cluster <- penguins_cleaned[c("bill_length_mm", "bill_depth_mm")]

set.seed(2026)
km <- kmeans(penguins_cluster, 3)
km$centers
plot(penguins_cluster$bill_length_mm, 
     penguins_cluster$bill_depth_mm,
     col = km$cluster,
     pch = 17
     )

points(km$centers,
      col = 4,
      pch = 8,
      cex = 2.5,
      lwd = 3
       )
