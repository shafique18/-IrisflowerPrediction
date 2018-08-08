#first install caret package for explanaton visit http://topepo.github.io/caret/index.html
install.packages("caret")
install.packages("ellipse")

#Now, let's load the package
library(caret)

#Load The Data
#Fortunately, the R platform provides the iris dataset for us
# attach the iris dataset to the environment
data(iris)
# rename the dataset
dataset <- iris


#or we can Load From CSV
# load the CSV file from the local directory
dataset <- read.csv("provide here full file name with full path", header=FALSE)
# set the column names in the dataset
colnames(dataset) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")


#Create a Validation Dataset
# creating a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
# selecting 20% of the data for validation
validation <- dataset[-validation_index,]
#we will use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]



#Summarize Dataset
# dimensions of dataset#
dim(dataset)
# list types for each attribute
sapply(dataset, class)
# take a peek at the first 5 rows of the data
head(dataset)
# list the levels for the class
levels(dataset$Species)
# summarize the class distribution
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)
# summarize attribute distributions
summary(dataset)



#Visualize Dataset
# split input and output
x <- dataset[,1:4]
y <- dataset[,5]
# boxplot for each attribute on one image
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}
# barplot for class breakdown
plot(y)
# scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")
# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")
# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)



#Evaluate Some Algorithms
#Test Harness
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#Build Models
# a) linear algorithms -  Linear Discriminant Analysis (LDA).
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms 
# CART - Classification and Regression Trees (CART).
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN - k-Nearest Neighbors (kNN).
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# Support Vector Machines (SVM)
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest (RF)
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)



#Select Best Model
#summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)


#compare accuracy of models
dotplot(results)



# summarize Best Model
print(fit.lda)


#Make Predictions
# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
