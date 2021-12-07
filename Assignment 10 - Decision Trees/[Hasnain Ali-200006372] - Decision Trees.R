library(rpart)
library(rpart.plot)

class_dataset <- read.csv("Path To Data File", header=FALSE)

ChocolateOrVanillaFunc <- function() { 
  tree1 = rpart(V6 ~ V8 + V7 + V4 + V10, data=class_dataset, method="class")
  rpart.plot(tree1)
  print(tree1)
  printcp(tree1)
}

ThermostatTemp <- function() { 
  tree2 = rpart(V10 ~ V3 + V7 + V4 + V6, data=class_dataset, method="class")
  rpart.plot(tree2)
  print(tree2)
  printcp(tree2)
}

DressColor <- function() { 
  tree3 = rpart(V3 ~ V6 + V7 + V4 + V10, data=class_dataset[2:100,], method="class")
  rpart.plot(tree3)
  print(tree3)
  printcp(tree3)
}

ChocolateOrVanillaFunc()
ThermostatTemp()
DressColor()