### This code was developed to perform post-estimation class calculations for the paper
### "Impact of the COVID-19 pandemic on the comfort of riding a crowded bus in Metro Vancouver, Canada" 
### The paper is available at https://doi.org/10.1016/j.tranpol.2023.07.018

### The code was developed by Bogdan Kapatsila in Spring-Summer 2022

rm(list = ls())

setwd("D:/Research/2020_TransLink_Overcrowding/Data/DCLV/Combined")

database = read.csv("translink_latent_class.csv",header=TRUE)

database <- subset(database, select=-c(X))
database <- database[order(database$UNID),]

# Classification estimates

gamma_LV1_female  = 0.31376 
gamma_LV1_age2544  = 0.27735
gamma_LV1_kids = 0.47939
gamma_LV1_ampeak = -0.25472

thre11 = -1.62402
thre12 = 0.55139

lambdaX1 = 1
lambdaX2 = 1

gamma_LV2_female  = -0.50887 
gamma_LV2_incomelow = 0.29563
gamma_LV2_age65O  =  -0.70508
gamma_LV2_edbach = 0.42408

thre2 = -0.06783

std = 1.25550


# Combine classes conditional on the common normally distributed error term

eta <- rnorm(10000)

LVX1 <- list()

VA1 <- list()
VB1 <- list()

piA1 <- list()
piB1 <- list()
piC1 <- list()

LVX2 <- list()

VA2 <- list()

piA2 <- list()
piB2 <- list()

piA1piA2 <- list()
piA1piB2 <- list()
piB1piA2 <- list()
piB1piB2 <- list()
piC1piA2 <- list()
piC1piB2 <- list()

for (k in 1:10000){

# LVX_1
  
  LVX1[[k]] <- gamma_LV1_female * database$FEMALE + gamma_LV1_age2544 * database$AGE_2544 + 
    gamma_LV1_kids * database$HOUSEHOLD_CHILD_CLEAN_N + gamma_LV1_ampeak * database$TIME_BC_AM + eta[k]*std
  
  VA1[[k]] = lambdaX1*(LVX1[[k]]-thre11)
  VB1[[k]] = lambdaX1*(LVX1[[k]]-thre12)
  
  piA1[[k]] = 1/(1 + exp(VA1[[k]]))
  piB1[[k]] = 1/(1 + exp(VB1[[k]])) - piA1[[k]]
  piC1[[k]] = 1-piA1[[k]]-piB1[[k]] 
  

# LVX_2
  
LVX2[[k]] = gamma_LV2_female * database$FEMALE + gamma_LV2_incomelow * database$INCOME_LOW + 
  gamma_LV2_age65O * database$AGE_65O + gamma_LV2_edbach * database$EDUCATION_BACH + eta[k]*std 

VA2[[k]]   = lambdaX2*(LVX2[[k]]-thre2)
piA2[[k]] = 1/(1 + exp(VA2[[k]]))
piB2[[k]] = 1 - piA2[[k]]

piA1piA2[[k]]=piA1[[k]]*piA2[[k]]
piA1piB2[[k]]=piA1[[k]]*piB2[[k]]
piB1piA2[[k]]=piB1[[k]]*piA2[[k]]
piB1piB2[[k]]=piB1[[k]]*piB2[[k]]
piC1piA2[[k]]=piC1[[k]]*piA2[[k]]
piC1piB2[[k]]=piC1[[k]]*piB2[[k]]

}

### Average across inter-individual draws

piA1piA2.mean <- colMeans(matrix(unlist(piA1piA2), nrow = length(piA1piA2), byrow = TRUE))
piA1piB2.mean <- colMeans(matrix(unlist(piA1piB2), nrow = length(piA1piB2), byrow = TRUE))
piB1piA2.mean <- colMeans(matrix(unlist(piB1piA2), nrow = length(piB1piA2), byrow = TRUE))
piB1piB2.mean <- colMeans(matrix(unlist(piB1piB2), nrow = length(piB1piB2), byrow = TRUE))
piC1piA2.mean <- colMeans(matrix(unlist(piC1piA2), nrow = length(piC1piA2), byrow = TRUE))
piC1piB2.mean <- colMeans(matrix(unlist(piC1piB2), nrow = length(piC1piB2), byrow = TRUE))

Prob_final <- data.frame(piA1piA2.mean, piA1piB2.mean, piB1piA2.mean, piB1piB2.mean, piC1piA2.mean, piC1piB2.mean)

# Save dataframe with individual probabilities

data_merge <- cbind(database, Prob_final)
write.csv(data_merge, 'translink_latent_class_prob_2.csv')

# Save average class probabilities

means <- data.frame(colMeans(Prob_final))
means$Class <- rownames(means)

means <- means[, c(2,1)]
rownames(means) <- 1:nrow(means)
names(means)[names(means) == 'colMeans.Prob_final.'] <- 'Probability'
write.csv(means, 'Final_6_Class_Means.csv')
