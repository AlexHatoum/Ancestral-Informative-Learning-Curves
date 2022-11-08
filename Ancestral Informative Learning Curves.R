#The script is fairly long, but almost all of it you can keep as is! If you name your ancestry variable "pheno" and 
#your predicted outcome "opidep" you can just keep almost all of it as is, and only mess with two lines in the loop. 
#The statement ******CHANGE********* tells you were there is something important you may need to change
#To get this working with your data

#packages you *need* depending on ML method, may need more. 
library(pROC)
library(caret)
#I like to dummy code categorical variables. 
library(fastDummies)

#Get your data
#******CHANGE*********
FullSet <- read.csv("WideFormData.txt", header=T)
#Your data should contain only an ID column, a column that represents train and test, your outcome
#Your predictor and your confounder (in my case, african american or european). To do this method, its 
#best if you only have two level variables for your confounder and your outcome. 

#Split into Train and Test
Train <- FullSet[FullSet$Set == "TRAIN", ]
Test <- FullSet[FullSet$Set == "TEST", ] 

#Only use complete cases
#******CHANGE********* to the name of your outcome variable after the $
Train <- Train[complete.cases(Train$OUD), ]
Test <- Test[complete.cases(Test$OUD), ]

#Get rid of the train and test and name your name your ancestry variable  "pheno" and your outcome variable. My outcome is labeled "opidep" 
#Because my curve is predicting opioid dependence. 
Train$ML_ID <- NULL
Train$Set <- NULL
Train$Euro_proportion <- NULL
#******CHANGE********* to what you want your ancestry variable and outcome to be, just make sure to change this throughout (or use my labels!)
colnames(Train)[1] <- c("pheno")
colnames(Train)[13] <- c("opidep")

#dummy code columns, if uyo uneed to
#Train1 <- dummy_cols(Train)

#Do what you did for train, for the test set
Test$ML_ID <- NULL
Test$Set <- NULL
Test$Euro_proportion <- NULL
colnames(Test)[1] <- c("pheno")
colnames(Test)[13] <- c("opidep")
colnames(TesterB)[1] <- "pheno"


#Create a seperate file for African Americans and European Americans in Trainer and Test
TrainerAA <- TrainerB[TrainerB$pheno == 0, ]
TesterAA <- TesterB[TesterB$pheno == 0, ]

TrainerEur <- TrainerB[TrainerB$pheno == 1,]
TesterEur <- TesterB[TesterB$pheno == 1,]

#Create a case and control african american and a case control european data frame
TrainerAAS <- TrainerAA[TrainerAA$opidep== 2, ]
TrainerAAL <- TrainerAA[TrainerAA$opidep == 1, ]
TrainerEurS <- TrainerEur[TrainerEur$opidep== 2, ]
TrainerEurL <- TrainerEur[TrainerEur$opidep == 1, ]

TesterAAS <- TesterAA[TesterAA$opidep== 2, ]
TesterAAL <- TesterAA[TesterAA$opidep == 1, ]
TesterEurS <- TesterEur[TesterEur$opidep== 2, ]
TesterEurL <- TesterEur[TesterEur$opidep == 1, ]

#Have another test dataframe that has your confounder in it for later. After the next step your file
#Will only have the outcome and predictors in it. 
TesterAAS2 <- TesterAAS
TesterAAL2 <- TesterAAL
TesterEurS2 <- TesterEurS
TesterEurL2 <- TesterEurL

TrainerAAS$pheno <- NULL
TrainerAAL$pheno <- NULL
TrainerEurS$pheno <- NULL
TrainerEurL$pheno <- NULL

TesterAAS$pheno <- NULL
TesterAAL$pheno <- NULL
TesterEurS$pheno <- NULL
TesterEurL$pheno <- NULL

mEurS <- NULL
mEurT <- NULL
mAAS <- NULL
mAAT <- NULL
m <- NULL
AUCTrain <- NULL
AUCTest <-  NULL
Trainer <- NULL
i <- 1
#******CHANGE*********you need to change the second number to whatever the smallest denominator of 10 is from TrainerAAS, TrainerEurS, TrainerAAL, TrainerEurL
a <- seq(0, 140, 10)
#
AUC_Opi <- NULL
AUC_AA <- NULL
for (i in i:length(a)) {
  
  m <- a[i]
  Trainer1 <- rbind.data.frame(TrainerEurS, TrainerAAL)
  Trainer2 <- rbind.data.frame(TrainerAAS[1:m,], TrainerEurL[1:m,])
  Trainer3 <- rbind.data.frame(Trainer1, Trainer2)
  
  Tester1 <- rbind.data.frame(TesterEurS, TesterAAL)
  Tester2 <- rbind.data.frame(TesterAAS[1:m,], TesterEurL[1:m,])
  Tester3 <- rbind.data.frame(Tester1, Tester2)
  
  Cov1 <- rbind.data.frame(TesterEurS2, TesterAAL2)
  Cov2 <- rbind.data.frame(TesterAAS2[1:m,], TesterEurL2[1:m,])
  Cov1$opidep <- NULL
  Cov2$opidep <- NULL
  Cov3 <- rbind.data.frame(Cov1, Cov2)
  Cov3$pheno <- as.integer((ifelse(Cov3$pheno == 1, 2, 1)))
  set.seed(3233)
  #####Change, caret control stratement. Change to the parameters you want to train wih
  ctrl1 <-  trainControl(method="cv", number=10, allowParallel = TRUE, search="random")
  #Change to the type of algrothim you want, "method=", and the type of grid you would like (mine is random here so I just
  #Do a tune length of 15, that often gets the job done)
  #Right now I am just using glmnet because it is fast
  CvExtra3.mod <- train(as.factor(opidep) ~ ., data=Trainer3, method = "glmnet",
                        trcontrol="ctrl1", tunelength=15,
                        allowParallel = TRUE)
  
  CvExtra3.PredC <- predict(CvExtra3.mod, Tester3)
  AUC_Outcome[i] <- AUC(as.numeric(as.character(CvExtra3.PredC)), as.numeric(as.character(Tester3$opidep)))
  AUC_Ancestry[i] <- AUC(as.numeric(as.character(CvExtra3.PredC)), as.numeric(as.character(Cov3$pheno)))
}
GLMNET_Curve_Random <- cbind.data.frame(a, AUC_Outcome, AUC_AA)
#AUC_Outcome is the prediction of your outcome and AUC_Ancestry is the predictoin of ancestry
write.csv(GLMNET_Curve_Random, "CurveOut.csv")


#plot! ****CHANGE***** this is ggplot and you will need to customize this
library(ggplot2)
ggplot(data = GLMNET_Curve_Random, aes(a, AUC_Opi)) + 
  geom_point(aes(x = a, y = AUC_Opi), legend=  TRUE,  xlab="X", ylab="Y", colour=alpha('red', 0.5)) + 
  geom_point(aes(x = a, y = AUC_AA), legend = TRUE, colour=alpha('blue', 0.55) ) +
  labs(y="AUC", x = "Sample size increase for Case and Control") 
