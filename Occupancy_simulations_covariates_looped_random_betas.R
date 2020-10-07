# README ####
# In this version effect sizes (beta coefficients) are randomly generated
# Purpose of multiple simulations is to show that parameter recovery over different model structures is not due
# to the specific effect sizes chosen

# Simulation parameters ####
n <- 3 # Number of times simulation will run
set.seed(135) # Seed for random numbers

# Study design ####
sites <- 3000 # Number of sites surveyed
K <- 40 # Number of times each site is surveyed (e.g. no. camera nights)
det.prob <- 0.5 # Probability of detection does not depend on any covariates in this model

# Covariate dependence structure ####
fig2 <- graph.formula(X -+ Psi, A -+ X, A -+ B, C -+ B, C -+ Psi,
                      simplify = FALSE)


#  Empty list to store results of each run ####
resultsgood <- list()
resultsbad <- list()

# Simulation loop ####
# Run simulation n times
for(run in 1:n){
  
  # True effect sizes (beta values and intercept) NB. these will be on logit scale ####
  # True effect sizes selected randomly between -1 and 1
  alpha <- runif(1,min=0, max=1) # Intercept value for psi
  B1 <- runif(1,min=0, max=1) # Effect of X on Psi
  B2 <- runif(1,min=0, max=1) # Effect of A on X
  B3 <- runif(1,min=0, max=1) # Effect of A on B
  B4 <- runif(1,min=0, max=1) # Effect of C on B
  B5 <- runif(1,min=0, max=1) # Effect of C on Psi
  
  # Explanatory variable values ####
  A <- rnorm(sites,4,1)
  C <- rnorm(sites,3,1)
  
  # Response variable values other than psi ####
  X <- 0 + (B2*A) + rnorm(sites,0,0.1)
  B <- 0 + (B3*A) + (B4*C) + rnorm(sites,0,0.1)
  
  # True psi value ####
  logit_psi_t <- alpha + (B1*X) + (B5*C) 
  psi_t <- inv.logit(logit_psi_t)
  
  # Bind Psi and covariates into a "landscape" (dataframe) ####
  grid <- cbind(psi_t, logit_psi_t, X, A, B, C)
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
      det <- cbind(det, rbinom(dim(df)[1],1,prob = df$occ * det.prob))
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
    habitatcov <- df %>% select(X, A, B, C)
    det <- df %>% select(starts_with("S",ignore.case = F))
    um_grid <- unmarkedFrameOccu(y=det,siteCovs = habitatcov)
  }
  # Make unmarked dataframe
  um_grid <- makeunmarked(grid)
  
  # Run occupancy model ####
  # Define good and bad model function
  mod_fun_good <- function(df){
    occu(~ 1 ~ X, linkPsi="logit",data=df)
  }
  
  mod_fun_bad <- function(df){
    occu(~ 1 ~ X + B, linkPsi="logit",data=df)
  }
  
  # Run the model and get the summary
  mgood <- mod_fun_good(um_grid)
  sumgood <- summary(mgood)
  
  mbad <- mod_fun_bad(um_grid)
  sumbad <- summary(mbad)
  
  # Convert summary table into dataframe 
  B1df <- as.data.frame(rep(B1, times=1))
  
  sumgooddf <- as.data.frame(t(unlist(sumgood)))
  sumgooddf <- cbind(sumgooddf,B1df)
  colnames(sumgooddf) <- c("Intercept.est","X.est","Intercept.se","X.se","Intercept.zscore","X.zscore",
                           "Intercept.pval","X.pval","det.est","det.se","det.z","det.pval","B1")
  
  sumbaddf <- as.data.frame(t(unlist(sumbad)))
  sumbaddf <- cbind(sumbaddf,B1df)
  colnames(sumbaddf) <- c("Intercept.est","X.est","B.est","Intercept.se","X.se","B.se","Intercept.zscore","X.zscore",
                          "B.zscore","Intercept.pval","X.pval","B.pval","det.est","det.se","det.z","det.pval","B1")
  
  # Append summary table dataframe to master results 
  run <- paste('run:',run,sep='')
  
  resultsgood[[run]] <- sumgooddf
  resultsbad[[run]] <- sumbaddf
}

# Turn results list into a dataframe ####
resultsdf_good <- bind_rows(resultsgood, .id = "column_label")
resultsdf_bad <- bind_rows(resultsbad, .id = "column_label")

#save(resultsdf_good, file="occusim_mbias_good.Rdata")
#save(resultsdf_bad, file="occusim_mbias_bad.Rdata")


# Calculate 95% confidence intervals ####
resultsdf_good$X.ci.up <- resultsdf_good$X.est + 1.96*resultsdf_good$X.se
resultsdf_good$X.ci.lo <- resultsdf_good$X.est - 1.96*resultsdf_good$X.se
resultsdf_bad$X.ci.up <- resultsdf_bad$X.est + 1.96*resultsdf_bad$X.se
resultsdf_bad$X.ci.lo <- resultsdf_bad$X.est - 1.96*resultsdf_bad$X.se

# Is the true value within the confidence interval?
resultsdf_good$X.inci <- ifelse(B1 <= resultsdf_good$X.ci.up,ifelse(resultsdf_good$X.ci.lo <= B1,1,0),0)
resultsdf_bad$X.inci <- ifelse(B1 <= resultsdf_bad$X.ci.up,ifelse(resultsdf_bad$X.ci.lo <= B1,1,0),0)

summary(resultsdf_good$X.inci)
summary(resultsdf_bad$X.inci)

# How far are the estimates from the true value?
resultsdf_good$bias <- B1 - resultsdf_good$X.est
resultsdf_bad$bias <- B1 - resultsdf_bad$X.est