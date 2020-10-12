# Load data
setwd("C:/Users/PeteS/OneDrive/R Scripts Library/Projects/Durham Occupancy Simulations/For thesis")

K7det01 <- get(load("Psi_onecov_K7_det01.Rdata"))
K7det03 <- get(load("Psi_onecov_K7_det03.Rdata"))
K7det05 <- get(load("Psi_onecov_K7_det05.Rdata"))

K14det01 <- get(load("Psi_onecov_K14_det01.Rdata"))
K14det03 <- get(load("Psi_onecov_K14_det03.Rdata"))
K14det05 <- get(load("Psi_onecov_K14_det05.Rdata"))

K21det01 <- get(load("Psi_onecov_K21_det01.Rdata"))
K21det03 <- get(load("Psi_onecov_K21_det03.Rdata"))
K21det05 <- get(load("Psi_onecov_K21_det05.Rdata"))

# Add sites and K columns where not already present
K7det03$sites <- 267
K7det03$K <- 7
K7det05$sites <- 267
K7det05$K <- 7 

K21det05$sites <- 89
K21det05$K <- 21

K14det05$sites <- 133
K14det05$K <- 14


# Add columns for detection probability
K21det05$true.pdet <- 0.5 
K14det05$true.pdet <- 0.5 
K7det05$true.pdet <- 0.5 
K21det05$true.pdet.logit <- 0
K14det05$true.pdet.logit <- 0
K7det05$true.pdet.logit <- 0

K21det03$true.pdet <- 0.3
K14det03$true.pdet <- 0.3
K7det03$true.pdet <- 0.3
K21det03$true.pdet.logit <- -0.8472979
K14det03$true.pdet.logit <- -0.8472979
K7det03$true.pdet.logit <- -0.8472979

K21det01$true.pdet <- 0.1
K14det01$true.pdet <- 0.1
K7det01$true.pdet <- 0.1
K21det01$true.pdet.logit <- -2.197225
K14det01$true.pdet.logit <- -2.197225
K7det01$true.pdet.logit <- -2.197225

# Combine results into a single dataframe and save
resultsall <- rbind(K7det01,
                    K7det03,
                    K7det05,
                    K14det01,
                    K14det03,
                    K14det05,
                    K21det01,
                    K21det03,
                    K21det05)

save(resultsall, file="Psi_onecov_all.Rdata")
