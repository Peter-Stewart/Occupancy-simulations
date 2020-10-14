# README ####
# In this version effect sizes (beta coefficients) are randomly generated
# Purpose of multiple simulations is to show that parameter recovery over different model structures is not due
# to the specific effect sizes chosen


# Packages ####
library(igraph)
library(boot)
library(plyr)
library(dplyr)
library(unmarked)
library(beepr)

# Simulation parameters ####
n <- 10000 # Number of times simulation will run
set.seed(135) # Seed for random numbers

# Study design ####
sites <- 267 # Number of sites surveyed
K <- 7 # Number of times each site is surveyed (e.g. no. camera nights)

# Covariate dependence structure ####
fig2 <- graph.formula(X -+ Psi,
                      Q -+ Pdet, R -+ Pdet,
                      simplify = FALSE)

# True effect sizes (beta values and intercept) NB. these will be on logit scale ####
# For occupancy
alpha <- 0 # Intercept value for psi (psi in habitat A = 0.50)

# For detection - no covariates in this simulation


#  Empty lists to store results of each run ####
results1 <- list()
results2 <- list()
results3 <- list()
results4 <- list()

# Simulation loop ####
# Run simulation n times
ptm<-proc.time()
for(run in 1:n){
  
  # Random beta coefficient for effect of X on Psi
  B1 <- runif(1, min = -3, max = 3) # Effect of X on Psi (changes psi from 0.05 to 0.95)
  
  # Random beta coefficients for effects of Q and R on Pdet
  B2 <- runif(1, min = -0.5, max = 0.5)
  B3 <- runif(1, min = -0.5, max = 0.5)
  
  # Explanatory variable values ####
  # For occupancy
  X <- rbinom(n=sites,size=1,prob=0.5)
  
  # For detection
  Q <- rnorm(n=sites, mean=0,sd=1)
  R <- rnorm(n=sites, mean=0,sd=1)
  
  # Response variable values other than psi and p ####
 
  
  # True psi value ####
  logit_psi_t <- alpha + (B1*X)
  psi_t <- inv.logit(logit_psi_t)
  
  # True probability of detection (p) ####
  logit.det.prob <-  0 + (B2*Q) + (B3*R)
  det.prob <- inv.logit(logit.det.prob)
  
  
  # Bind Psi and covariates into a "landscape" (dataframe) ####
  grid <- cbind(psi_t, logit_psi_t, X, det.prob, logit.det.prob, Q, R)
  grid <- as.data.frame(grid)

  # Simulate true occupancy in each landscape using psi ####
  # Define function to occupy the landscape
  occupy <- function(df){
    occ <- rbinom(nrow(df),size=1,prob=df$psi_t)
    cbind(df,occ)
  }
  # Occupy the landscape
  grid <- occupy(grid)
  
  # Simulate detection histories using true occupancy and probability of detection for K sampling events ####
  # Define function to simulate detection histories
  makedetections <- function(df){
    det <- NULL
    for (i in 1:K){
      det <- cbind(det, rbinom(dim(df)[1],1,prob = df$occ * df$det.prob))
    }
    det<- as.data.frame(det)
    varname <- "S"  # Select prefix for column names
    names(det)[1:ncol(det)] <- unlist(mapply(function(x,y) paste(x, seq(1,y), sep=""), varname, K)) # Rename columns to S1, S2 etc.
    df <- cbind(df,det) # Recombine detection history with true probabilities 
    # Add site ID numbers
    site <- seq(1:nrow(df))
    df <- cbind(site,df)
  }
  # Generate detection histories
  grid <- makedetections(grid)
  
  # Convert landscape into unmarked dataframe for the model to use ####
  # Define function to make unmarked dataframe
  makeunmarked <- function(df){
    habitatcov <- df %>% select(X,Q,R)
    det <- df %>% select(starts_with("S",ignore.case = F))
    um_grid <- unmarkedFrameOccu(y=det,siteCovs = habitatcov)
  }
  # Make unmarked dataframe
  um_grid <- makeunmarked(grid)
  
  # Run occupancy model ####
  # Define model functions
  mod_fun1 <- function(df){
    occu(~ 1 ~ X, linkPsi="logit",data=df)
  }
  
  mod_fun2 <- function(df){
    occu(~ Q ~ X, linkPsi="logit",data=df)
  }
  
  mod_fun3 <- function(df){
    occu(~ R ~ X, linkPsi="logit",data=df)
  }

  mod_fun4 <- function(df){
    occu(~ Q + R ~ X, linkPsi="logit",data=df)
  }
  
  # Run the models and get the summaries
  m1 <- mod_fun1(um_grid)
  sum1 <- summary(m1)

  m2 <- mod_fun2(um_grid)
  sum2 <- summary(m2)
  
  m3 <- mod_fun3(um_grid)
  sum3 <- summary(m3)
  
  m4 <- mod_fun4(um_grid)
  sum4 <- summary(m4)
  
  # Convert summary tables into dataframe 
  sumdf1 <- as.data.frame(t(unlist(sum1)))
  colnames(sumdf1) <- c("Intercept.est","X.est","Intercept.se","X.se","Intercept.zscore","X.zscore",
                       "Intercept.pval","X.pval",
                       "det.intercept.est",
                       "det.intercept.se",
                       "det.intercept.zscore",
                       "det.intercept.pval")
        
  sumdf2 <- as.data.frame(t(unlist(sum2)))
  colnames(sumdf2) <- c("Intercept.est","X.est","Intercept.se","X.se","Intercept.zscore","X.zscore",
                        "Intercept.pval","X.pval",
                        "det.intercept.est",
                        "Q.est",
                        "det.intercept.se",
                        "Q.se",
                        "det.intercept.zscore",
                        "Q.zscore",
                        "det.intercept.pval",
                        "Q.pval")
  
  sumdf3 <- as.data.frame(t(unlist(sum3)))
  colnames(sumdf3) <- c("Intercept.est","X.est","Intercept.se","X.se","Intercept.zscore","X.zscore",
                        "Intercept.pval","X.pval",
                        "det.intercept.est",
                        "R.est",
                        "det.intercept.se",
                        "R.se",
                        "det.intercept.zscore",
                        "R.zscore",
                        "det.intercept.pval",
                        "R.pval")
  
  sumdf4 <- as.data.frame(t(unlist(sum4)))
  colnames(sumdf4) <- c("Intercept.est","X.est","Intercept.se","X.se","Intercept.zscore","X.zscore",
                        "Intercept.pval","X.pval",
                        "det.intercept.est",
                        "Q.est",
                        "R.est",
                        "det.intercept.se",
                        "Q.se",
                        "R.se",
                        "det.intercept.zscore",
                        "Q.zscore",
                        "R.zscore",
                        "det.intercept.pval",
                        "Q.pval",
                        "R.pval")
  
  B1df <- as.data.frame(rep(B1, times=1))
  colnames(B1df) <- "B1"
  sumdf1 <- cbind(sumdf1,B1df)
  sumdf2 <- cbind(sumdf2,B1df)
  sumdf3 <- cbind(sumdf3,B1df)
  sumdf4 <- cbind(sumdf4,B1df)
  
  # Append summary table dataframe to master results 
  run <- paste('run:',run,sep='')
  
  results1[[run]] <- sumdf1
  results2[[run]] <- sumdf2
  results3[[run]] <- sumdf3
  results4[[run]] <- sumdf4
}

# Turn results list into a dataframe ####
resultsdf1 <- bind_rows(results1, .id = "column_label")
resultsdf2 <- bind_rows(results2, .id = "column_label")
resultsdf3 <- bind_rows(results3, .id = "column_label")
resultsdf4 <- bind_rows(results4, .id = "column_label")


# How long did that take?
new.time<-proc.time()-ptm
beep(sound=3,expr=NULL)
new.time[3]/60

# Add columns for sites and K
resultsdf1$sites <- sites
resultsdf1$K <- K
resultsdf2$sites <- sites
resultsdf2$K <- K
resultsdf3$sites <- sites
resultsdf3$K <- K
resultsdf4$sites <- sites
resultsdf4$K <- K

# Combine all dataframes into one
resultsdf1$detcovs <- as.factor("none")
resultsdf2$detcovs <- as.factor("Q")
resultsdf3$detcovs <- as.factor("R")
resultsdf4$detcovs <- as.factor("QandR")

resultsdf <- rbind.fill(resultsdf1,resultsdf2,resultsdf3,resultsdf4)

# Save results
save(resultsdf,file="C:/Users/PeteS/OneDrive/R Scripts Library/Projects/Durham Occupancy Simulations/Psi_onecov_det_twocov_K7.Rdata")





