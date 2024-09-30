library(datasets)
library(MASS)

iris = iris
head(iris)

#model DA
model1 = lda(Species~., data=iris)
model1
