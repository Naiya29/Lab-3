library(dplyr)
library(tidyverse)  
library(factoextra)  
library(caret)
library(cluster)
library(ggplot2)

student_data <- read.csv("C:\\Users\\patel\\Downloads\\oulad-students.csv")

str(student_data)
summary(student_data)

student_data$final_result <- as.factor(student_data$final_result)
student_data$gender <- as.numeric(student_data$gender)
student_data$id_student <- as.numeric(student_data$id_student)

# learning data
set.seed(123)
n_students <- 1000
student_identifier <- 1:n_students
cognitive_ability <- rnorm(n_students, mean = 50, sd = 10)
learning_style <- rnorm(n_students, mean = 70, sd = 15)
student_data <- data.frame(Student_ID = student_identifier, Cognitive_Ability = cognitive_ability, Learning_Style = learning_style)

# Perform PCA for dimensionality
student_pca <- prcomp(student_data[,c("Cognitive_Ability", "Learning_Style")], center = TRUE, scale. = TRUE)

# PCA results
library(factoextra)
fviz_eig(student_pca) + ggtitle("Scatter Plot")
fviz_pca_var(student_pca) + ggtitle("Variable Loading Plot")
fviz_pca_biplot(student_pca, repel = TRUE, title = "Biplot")

# K-means clustering
set.seed(123)
kmeans <- kmeans(pca$x[,1:2], centers = 6, nstart = 20)

# Plot the clustering
fviz_cluster(kmeans, student_data[,c("Cognitive_Ability", "Learning_Style")]) + ggtitle("Clustering Results")

# Add cluster labels
student_data$Cluster <- as.factor(kmeans$cluster)

# clustering results
summary(student_data$Cluster)
