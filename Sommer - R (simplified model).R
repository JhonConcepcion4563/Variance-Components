#Genetic Correlation

setwd("E:/DON_Pheno_Geno_Prediction/Forward Predictions/Forward Predictions - Pred vs Obs")

data <- read.table("2021-Predict-2022-Pred-vs-Obs.csv", header=T, sep=",")

library(sommer)
library(lme4)

#Note: All predicted values used are from model averaging
trait1 <- data[, 2]
trait2 <- data[, 3]#3 = Phenomics, 4 = Genomics, 5 = Phenomics+Genomics
entry_ids <- data[, 1]

###################################################################
#USE THIS
model_data <- data.frame(
  entry_id = factor(entry_ids),
  trait1 = trait1,
  trait2 = trait2
)

model <- mmer(cbind(trait1, trait2) ~ 1,
              random = ~ vsr(entry_id, Gtc=unsm(2)),
              rcov = ~ vsr(units,Gtc=diag(2)),
              data = model_data, verbose=FALSE)

summary(model) #to get the variance components

var(trait2) #to get the phenotypic variance