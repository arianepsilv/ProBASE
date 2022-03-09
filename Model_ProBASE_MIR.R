##########################
###### import data #######
##########################
# Dados de modelagem
setwd("C:/Users/arian/OneDrive/Documentos/Probase1/Artigo2/Submissao/Revis??o")
banco.geral       <- read.csv("MIR.csv"    , sep = ";", header = T)
banco.spectra.geral  <- data.frame(banco.geral[ ,29:3459])

##########################
######## library #########
##########################
library(gridExtra)  
library(ggplot2)
library(pls)
library(beepr)
library(devtools)
library(fBasics)  
library(fitdistrplus)
library(caret)
library(pastecs)
library(Cubist)
library(chillR)
library(ModelMetrics)

############Sand##############

banco.Sand  <- data.frame(cbind(banco.geral[,c(1,2)], banco.spectra.geral[ ,c(seq(1,3431))]))
banco.Sand  <- na.omit(banco.Sand)  
colnames(banco.Sand)[2] <- "variable"

# SPLIT #
set.seed(2)
inTrain    <- sample(1:nrow(banco.Sand), floor(.7*nrow(banco.Sand)))
training   <- banco.Sand[ inTrain,]
validation <- banco.Sand[-inTrain,]

model_Sand <- cubist(y = training$variable, x = training[ ,3:length(training)], committees =  10, .neighbors =  9)
model_Sand

# training
predi.train <- predict(model_Sand, training)
cor (predi.train, training$variable)^2 # R2
rmse(predi.train, training$variable)   # RMSE
RPIQ(predi.train, training$variable)   # rpiq 

# validation
predi.val <- predict(model_Sand, validation)
cor (predi.val, validation$variable)^2 # R2
rmse(predi.val, validation$variable)   # RMSE
RPIQ(predi.val, validation$variable)   # rpiq 

#######Silt######

banco.Silt  <- data.frame(cbind(banco.geral[,c(1,3)], banco.spectra.geral[ ,c(seq(1,3431))]))
banco.Silt  <- na.omit(banco.Silt)  
colnames(banco.Silt)[2] <- "variable"

# SPLIT #
set.seed(2)
inTrain    <- sample(1:nrow(banco.Silt), floor(.7*nrow(banco.Silt)))
training   <- banco.Silt[ inTrain,]
validation <- banco.Silt[-inTrain,]

model_Silt <- cubist(y = training$variable, x = training[ ,3:length(training)], committees =  10, .neighbors =  9)
model_Silt

# training
predi.train <- predict(model_Silt, training)
cor (predi.train, training$variable)^2 # R2
rmse(predi.train, training$variable)   # RMSE
RPIQ(predi.train, training$variable)   # rpiq 

# validation
predi.val <- predict(model_Silt, validation)
cor (predi.val, validation$variable)^2 # R2
rmse(predi.val, validation$variable)   # RMSE
RPIQ(predi.val, validation$variable)   # rpiq 

############## Clay ##############

banco.Clay  <- data.frame(cbind(banco.geral[,c(1,4)], banco.spectra.geral[ ,c(seq(1,3431))]))
banco.Clay  <- na.omit(banco.Clay)  
colnames(banco.Clay)[2] <- "variable"

# SPLIT #
set.seed(2)
inTrain    <- sample(1:nrow(banco.Clay), floor(.7*nrow(banco.Clay)))
training   <- banco.Clay[ inTrain,]
validation <- banco.Clay[-inTrain,]

model_Clay <- cubist(y = training$variable, x = training[ ,3:length(training)], committees =  10, .neighbors =  9)
model_Clay

# training
predi.train <- predict(model_Clay, training)
cor (predi.train, training$variable)^2 # R2
rmse(predi.train, training$variable)   # RMSE
RPIQ(predi.train, training$variable)   # rpiq 

# validation
predi.val <- predict(model_Clay, validation)
cor (predi.val, validation$variable)^2 # R2
rmse(predi.val, validation$variable)   # RMSE
RPIQ(predi.val, validation$variable)   # rpiq 

############ pH##########
banco.pH  <- data.frame(cbind(banco.geral[,c(1,5)], banco.spectra.geral[ ,c(seq(1,3431))]))
banco.pH  <- na.omit(banco.pH)  
colnames(banco.pH)[2] <- "variable"

# SPLIT #
set.seed(2)
inTrain    <- sample(1:nrow(banco.pH), floor(.7*nrow(banco.pH)))
training   <- banco.pH[ inTrain,]
validation <- banco.pH[-inTrain,]

model_pH <- cubist(y = training$variable, x = training[ ,3:length(training)], committees =  10, .neighbors =  9)
model_pH

# training
predi.train <- predict(model_pH, training)
cor (predi.train, training$variable)^2 # R2
rmse(predi.train, training$variable)   # RMSE
RPIQ(predi.train, training$variable)   # rpiq 

# validation
predi.val <- predict(model_pH, validation)
cor (predi.val, validation$variable)^2 # R2
rmse(predi.val, validation$variable)   # RMSE
RPIQ(predi.val, validation$variable)   # rpiq 

############## Organic Matter ###################
banco.OM  <- data.frame(cbind(banco.geral[,c(1,8)], banco.spectra.geral[ ,c(seq(1,3431))]))
banco.OM  <- na.omit(banco.OM)  
colnames(banco.OM)[2] <- "variable"

# SPLIT #
set.seed(2)
inTrain    <- sample(1:nrow(banco.OM), floor(.7*nrow(banco.OM)))
training   <- banco.OM[ inTrain,]
validation <- banco.OM[-inTrain,]

model_OM <- cubist(y = training$variable, x = training[ ,3:length(training)], committees =  10, .neighbors =  9)
model_OM

# training
predi.train <- predict(model_OM, training)
cor (predi.train, training$variable)^2 # R2
rmse(predi.train, training$variable)   # RMSE
RPIQ(predi.train, training$variable)   # rpiq 

# validation
predi.val <- predict(model_OM, validation)
cor (predi.val, validation$variable)^2 # R2
rmse(predi.val, validation$variable)   # RMSE
RPIQ(predi.val, validation$variable)   # rpiq 

###################### P ###########
banco.P  <- data.frame(cbind(banco.geral[,c(1,10)], banco.spectra.geral[ ,c(seq(1,3431))]))
banco.P  <- na.omit(banco.P)  
colnames(banco.P)[2] <- "variable"

# SPLIT #
set.seed(2)
inTrain    <- sample(1:nrow(banco.P), floor(.7*nrow(banco.P)))
training   <- banco.P[ inTrain,]
validation <- banco.P[-inTrain,]

model_P <- cubist(y = training$variable, x = training[ ,3:length(training)], committees =  10, .neighbors =  9)
model_P

# training
predi.train <- predict(model_P, training)
cor (predi.train, training$variable)^2 # R2
rmse(predi.train, training$variable)   # RMSE
RPIQ(predi.train, training$variable)   # rpiq 

# validation
predi.val <- predict(model_P, validation)
cor (predi.val, validation$variable)^2 # R2
rmse(predi.val, validation$variable)   # RMSE
RPIQ(predi.val, validation$variable)   # rpiq 


############### K ################################
banco.K  <- data.frame(cbind(banco.geral[,c(1,11)], banco.spectra.geral[ ,c(seq(1,3431))]))
banco.K  <- na.omit(banco.K)  
colnames(banco.K)[2] <- "variable"

# SPLIT #
set.seed(2)
inTrain    <- sample(1:nrow(banco.K), floor(.7*nrow(banco.K)))
training   <- banco.K[ inTrain,]
validation <- banco.K[-inTrain,]

model_K <- cubist(y = training$variable, x = training[ ,3:length(training)], committees =  10, .neighbors =  9)
model_K

# training
predi.train <- predict(model_K, training)
cor (predi.train, training$variable)^2 # R2
rmse(predi.train, training$variable)   # RMSE
RPIQ(predi.train, training$variable)   # rpiq 

# validation
predi.val <- predict(model_K, validation)
cor (predi.val, validation$variable)^2 # R2
rmse(predi.val, validation$variable)   # RMSE
RPIQ(predi.val, validation$variable)   # rpiq 

################## Ca  #######################
banco.Ca  <- data.frame(cbind(banco.geral[,c(1,12)], banco.spectra.geral[ ,c(seq(1,3431))]))
banco.Ca  <- na.omit(banco.Ca)  
colnames(banco.Ca)[2] <- "variable"

# SPLIT #
set.seed(2)
inTrain    <- sample(1:nrow(banco.Ca), floor(.7*nrow(banco.Ca)))
training   <- banco.Ca[ inTrain,]
validation <- banco.Ca[-inTrain,]

model_Ca <- cubist(y = training$variable, x = training[ ,3:length(training)], committees =  10, .neighbors =  9)
model_Ca

# training
predi.train <- predict(model_Ca, training)
cor (predi.train, training$variable)^2 # R2
rmse(predi.train, training$variable)   # RMSE
RPIQ(predi.train, training$variable)   # rpiq 

# validation
predi.val <- predict(model_Ca, validation)
cor (predi.val, validation$variable)^2 # R2
rmse(predi.val, validation$variable)   # RMSE
RPIQ(predi.val, validation$variable)   # rpiq 

#######################  Mg #####################
banco.Mg  <- data.frame(cbind(banco.geral[,c(1,13)], banco.spectra.geral[ ,c(seq(1,3431))]))
banco.Mg  <- na.omit(banco.Mg)  
colnames(banco.Mg)[2] <- "variable"

# SPLIT #
set.seed(2)
inTrain    <- sample(1:nrow(banco.Mg), floor(.7*nrow(banco.Mg)))
training   <- banco.Mg[ inTrain,]
validation <- banco.Mg[-inTrain,]

model_Mg <- cubist(y = training$variable, x = training[ ,3:length(training)], committees =  10, .neighbors =  9)
model_Mg

# training
predi.train <- predict(model_Mg, training)
cor (predi.train, training$variable)^2 # R2
rmse(predi.train, training$variable)   # RMSE
RPIQ(predi.train, training$variable)   # rpiq 

# validation
predi.val <- predict(model_Mg, validation)
cor (predi.val, validation$variable)^2 # R2
rmse(predi.val, validation$variable)   # RMSE
RPIQ(predi.val, validation$variable)   # rpiq 

###################  Al ###########################
banco.Al  <- data.frame(cbind(banco.geral[,c(1,14)], banco.spectra.geral[ ,c(seq(1,3431))]))
banco.Al  <- na.omit(banco.Al)  
colnames(banco.Al)[2] <- "variable"

# SPLIT #
set.seed(2)
inTrain    <- sample(1:nrow(banco.Al), floor(.7*nrow(banco.Al)))
training   <- banco.Al[ inTrain,]
validation <- banco.Al[-inTrain,]

model_Al <- cubist(y = training$variable, x = training[ ,3:length(training)], committees =  10, .neighbors =  9)
model_Al

# training
predi.train <- predict(model_Al, training)
cor (predi.train, training$variable)^2 # R2
rmse(predi.train, training$variable)   # RMSE
RPIQ(predi.train, training$variable)   # rpiq 

# validation
predi.val <- predict(model_Al, validation)
cor (predi.val, validation$variable)^2 # R2
rmse(predi.val, validation$variable)   # RMSE
RPIQ(predi.val, validation$variable)   # rpiq 

##################  H + Al ##########################
banco.HAl  <- data.frame(cbind(banco.geral[,c(1,16)], banco.spectra.geral[ ,c(seq(1,3431))]))
banco.HAl  <- na.omit(banco.HAl)  
colnames(banco.HAl)[2] <- "variable"

# SPLIT #
set.seed(2)
inTrain    <- sample(1:nrow(banco.HAl), floor(.7*nrow(banco.HAl)))
training   <- banco.HAl[ inTrain,]
validation <- banco.HAl[-inTrain,]

model_HAl <- cubist(y = training$variable, x = training[ ,3:length(training)], committees =  10, .neighbors =  9)
model_HAl

# training
predi.train <- predict(model_HAl, training)
cor (predi.train, training$variable)^2 # R2
rmse(predi.train, training$variable)   # RMSE
RPIQ(predi.train, training$variable)   # rpiq 

# validation
predi.val <- predict(model_HAl, validation)
cor (predi.val, validation$variable)^2 # R2
rmse(predi.val, validation$variable)   # RMSE
RPIQ(predi.val, validation$variable)   # rpiq 

##################### Sum Bases #####################
banco.SB  <- data.frame(cbind(banco.geral[,c(1,17)], banco.spectra.geral[ ,c(seq(1,3431))]))
banco.SB  <- na.omit(banco.SB)  
colnames(banco.SB)[2] <- "variable"

# SPLIT #
set.seed(2)
inTrain    <- sample(1:nrow(banco.SB), floor(.7*nrow(banco.SB)))
training   <- banco.SB[ inTrain,]
validation <- banco.SB[-inTrain,]

model_SB <- cubist(y = training$variable, x = training[ ,3:length(training)], committees =  10, .neighbors =  9)
model_SB

# training
predi.train <- predict(model_SB, training)
cor (predi.train, training$variable)^2 # R2
rmse(predi.train, training$variable)   # RMSE
RPIQ(predi.train, training$variable)   # rpiq 

# validation
predi.val <- predict(model_SB, validation)
cor (predi.val, validation$variable)^2 # R2
rmse(predi.val, validation$variable)   # RMSE
RPIQ(predi.val, validation$variable)   # rpiq 

############## CEC ###########################
banco.CEC  <- data.frame(cbind(banco.geral[,c(1,18)], banco.spectra.geral[ ,c(seq(1,3431))]))
banco.CEC  <- na.omit(banco.CEC)  
colnames(banco.CEC)[2] <- "variable"

# SPLIT #
set.seed(2)
inTrain    <- sample(1:nrow(banco.CEC), floor(.7*nrow(banco.CEC)))
training   <- banco.CEC[ inTrain,]
validation <- banco.CEC[-inTrain,]

model_CEC <- cubist(y = training$variable, x = training[ ,3:length(training)], committees =  10, .neighbors =  9)
model_CEC

# training
predi.train <- predict(model_CEC, training)
cor (predi.train, training$variable)^2 # R2
rmse(predi.train, training$variable)   # RMSE
RPIQ(predi.train, training$variable)   # rpiq 

# validation
predi.val <- predict(model_CEC, validation)
cor (predi.val, validation$variable)^2 # R2
rmse(predi.val, validation$variable)   # RMSE
RPIQ(predi.val, validation$variable)   # rpiq 

############## V ###########################
banco.V  <- data.frame(cbind(banco.geral[,c(1,19)], banco.spectra.geral[ ,c(seq(1,3431))]))
banco.V  <- na.omit(banco.V)  
colnames(banco.V)[2] <- "variable"

# SPLIT #
set.seed(2)
inTrain    <- sample(1:nrow(banco.V), floor(.7*nrow(banco.V)))
training   <- banco.V[ inTrain,]
validation <- banco.V[-inTrain,]

model_V <- cubist(y = training$variable, x = training[ ,3:length(training)], committees =  10, .neighbors =  9)
model_V

# training
predi.train <- predict(model_V, training)
cor (predi.train, training$variable)^2 # R2
rmse(predi.train, training$variable)   # RMSE
RPIQ(predi.train, training$variable)   # rpiq 

# validation
predi.val <- predict(model_V, validation)
cor (predi.val, validation$variable)^2 # R2
rmse(predi.val, validation$variable)   # RMSE
RPIQ(predi.val, validation$variable)   # rpiq 

############## M ###########################
banco.M  <- data.frame(cbind(banco.geral[,c(1,21)], banco.spectra.geral[ ,c(seq(1,3431))]))
banco.M  <- na.omit(banco.M)  
colnames(banco.M)[2] <- "variable"

# SPLIT #
set.seed(3)
inTrain    <- sample(1:nrow(banco.M), floor(.7*nrow(banco.M)))
training   <- banco.M[ inTrain,]
validation <- banco.M[-inTrain,]

model_M <- cubist(y = training$variable, x = training[ ,3:length(training)], committees =  10, .neighbors =  9)
model_M

# training
predi.train <- predict(model_M, training)
cor (predi.train, training$variable)^2 # R2
rmse(predi.train, training$variable)   # RMSE
RPIQ(predi.train, training$variable)   # rpiq 

# validation
predi.val <- predict(model_M, validation)
cor (predi.val, validation$variable)^2 # R2
rmse(predi.val, validation$variable)   # RMSE
RPIQ(predi.val, validation$variable)   # rpiq 

############## S ###########################
banco.S  <- data.frame(cbind(banco.geral[,c(1,22)], banco.spectra.geral[ ,c(seq(1,3431))]))
banco.S  <- na.omit(banco.S)  
colnames(banco.S)[2] <- "variable"

# SPLIT #
set.seed(2)
inTrain    <- sample(1:nrow(banco.S), floor(.7*nrow(banco.S)))
training   <- banco.S[ inTrain,]
validation <- banco.S[-inTrain,]

model_S <- cubist(y = training$variable, x = training[ ,3:length(training)], committees =  10, .neighbors =  9)
model_S

# training
predi.train <- predict(model_S, training)
cor (predi.train, training$variable)^2 # R2
rmse(predi.train, training$variable)   # RMSE
RPIQ(predi.train, training$variable)   # rpiq 

# validation
predi.val <- predict(model_S, validation)
cor (predi.val, validation$variable)^2 # R2
rmse(predi.val, validation$variable)   # RMSE
RPIQ(predi.val, validation$variable)   # rpiq 

############## B ###########################
banco.B  <- data.frame(cbind(banco.geral[,c(1,24)], banco.spectra.geral[ ,c(seq(1,3431))]))
banco.B  <- na.omit(banco.B)  
colnames(banco.B)[2] <- "variable"

# SPLIT #
set.seed(2)
inTrain    <- sample(1:nrow(banco.B), floor(.7*nrow(banco.B)))
training   <- banco.B[ inTrain,]
validation <- banco.B[-inTrain,]

model_B <- cubist(y = training$variable, x = training[ ,3:length(training)], committees =  10, .neighbors =  9)
model_B

# training
predi.train <- predict(model_B, training)
cor (predi.train, training$variable)^2 # R2
rmse(predi.train, training$variable)   # RMSE
RPIQ(predi.train, training$variable)   # rpiq 

# validation
predi.val <- predict(model_B, validation)
cor (predi.val, validation$variable)^2 # R2
rmse(predi.val, validation$variable)   # RMSE
RPIQ(predi.val, validation$variable)   # rpiq 

############## Fe ###########################
banco.Fe  <- data.frame(cbind(banco.geral[,c(1,25)], banco.spectra.geral[ ,c(seq(1,3431))]))
banco.Fe  <- na.omit(banco.Fe)  
colnames(banco.Fe)[2] <- "variable"

# SPLIT #
set.seed(2)
inTrain    <- sample(1:nrow(banco.Fe), floor(.7*nrow(banco.Fe)))
training   <- banco.Fe[ inTrain,]
validation <- banco.Fe[-inTrain,]

model_Fe <- cubist(y = training$variable, x = training[ ,3:length(training)], committees =  10, .neighbors =  9)
model_Fe

# training
predi.train <- predict(model_Fe, training)
cor (predi.train, training$variable)^2 # R2
rmse(predi.train, training$variable)   # RMSE
RPIQ(predi.train, training$variable)   # rpiq 

# validation
predi.val <- predict(model_Fe, validation)
cor (predi.val, validation$variable)^2 # R2
rmse(predi.val, validation$variable)   # RMSE
RPIQ(predi.val, validation$variable)   # rpiq 

############## Cu ###########################
banco.Cu  <- data.frame(cbind(banco.geral[,c(1,26)], banco.spectra.geral[ ,c(seq(1,3431))]))
banco.Cu  <- na.omit(banco.Cu)  
colnames(banco.Cu)[2] <- "variable"

# SPLIT #
set.seed(2)
inTrain    <- sample(1:nrow(banco.Cu), floor(.7*nrow(banco.Cu)))
training   <- banco.Cu[ inTrain,]
validation <- banco.Cu[-inTrain,]

model_Cu <- cubist(y = training$variable, x = training[ ,3:length(training)], committees =  10, .neighbors =  9)
model_Cu

# training
predi.train <- predict(model_Cu, training)
cor (predi.train, training$variable)^2 # R2
rmse(predi.train, training$variable)   # RMSE
RPIQ(predi.train, training$variable)   # rpiq 

# validation
predi.val <- predict(model_Cu, validation)
cor (predi.val, validation$variable)^2 # R2
rmse(predi.val, validation$variable)   # RMSE
RPIQ(predi.val, validation$variable)   # rpiq 

############## Zn ###########################
banco.Zn  <- data.frame(cbind(banco.geral[,c(1,27)], banco.spectra.geral[ ,c(seq(1,3431))]))
banco.Zn  <- na.omit(banco.Zn)  
colnames(banco.Zn)[2] <- "variable"

# SPLIT #
set.seed(2)
inTrain    <- sample(1:nrow(banco.Zn), floor(.7*nrow(banco.Zn)))
training   <- banco.Zn[ inTrain,]
validation <- banco.Zn[-inTrain,]

model_Zn <- cubist(y = training$variable, x = training[ ,3:length(training)], committees =  10, .neighbors =  9)
model_Zn

# training
predi.train <- predict(model_Zn, training)
cor (predi.train, training$variable)^2 # R2
rmse(predi.train, training$variable)   # RMSE
RPIQ(predi.train, training$variable)   # rpiq 

# validation
predi.val <- predict(model_Zn, validation)
cor (predi.val, validation$variable)^2 # R2
rmse(predi.val, validation$variable)   # RMSE
RPIQ(predi.val, validation$variable)   # rpiq 

############## Mn ###########################
banco.Mn  <- data.frame(cbind(banco.geral[,c(1,28)], banco.spectra.geral[ ,c(seq(1,3431))]))
banco.Mn  <- na.omit(banco.Mn)  
colnames(banco.Mn)[2] <- "variable"

# SPLIT #
set.seed(2)
inTrain    <- sample(1:nrow(banco.Mn), floor(.7*nrow(banco.Mn)))
training   <- banco.Mn[ inTrain,]
validation <- banco.Mn[-inTrain,]

model_Mn <- cubist(y = training$variable, x = training[ ,3:length(training)], committees =  10, .neighbors =  9)
model_Mn

# training
predi.train <- predict(model_Mn, training)
cor (predi.train, training$variable)^2 # R2
rmse(predi.train, training$variable)   # RMSE
RPIQ(predi.train, training$variable)   # rpiq 

# validation
predi.val <- predict(model_Mn, validation)
cor (predi.val, validation$variable)^2 # R2
rmse(predi.val, validation$variable)   # RMSE
RPIQ(predi.val, validation$variable)   # rpiq 
