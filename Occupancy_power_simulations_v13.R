#### Packages
library(dplyr)
library(unmarked)
library(forcats)
library(ggplot2)
library(boot)
library(purrr)
library(magrittr)
library(tidyr)
library(splitstackshape)
library(stringr)
library(reshape)
library(cowplot)
library(hexbin)
library(ggridges)


# Simulation parameters
n <- 1000 # Number of times simulation will run
set.seed(135) # Seed for random numbers

# Parameters which do not change
occ.prob.A <- 0.5 # True probability of occupancy in habitat A

det.prob.A<- 0.1 # True probability of detection in habitat A
det.prob.B <- 0.1 # True probability of detection in habitat B

K <- 7 # Number of camera nights (i.e. sampling occasions)

# Sequence of values for psi in habitat B
occ.prob.B <- seq(from=0,to=1,by=0.01)

#  Empty lists to store results of each run
allstateresults <- list()
alldetresults <- list()

# Run simulation n times
for(run in 1:n){
  
# Create list to store the landscapes
mastergrids <- list()

# Simulate landscapes
for (p in occ.prob.B){
  grid <- expand.grid(long=c(1:267),lat=1) # Create 89 sites
  grid$habitat <- rep(c(0,1),length.out=nrow(grid)) # Alternating habitat patches
  grid$habitat <- as.factor(grid$habitat) # Change habitat to factor
  grid$habitat <- fct_recode(grid$habitat,A="0", B="1") # Recode factor levels
  grid$prob <- rep(p,times=nrow(grid))
  iteration <- paste("grid",p,sep="")
  mastergrids[[iteration]] <- grid
}

# For each landscape, simulate true occupancy  
prepareprobs <- function(df){
  df$probnew <- ifelse(df$habitat=="B",df$prob,occ.prob.A)
}
mastergridsprobs <- map(mastergrids,prepareprobs)
d <- as.data.frame(t(mastergridsprobs))
mastergrids2 <- splice(mastergrids,d)
mastergrids2 <- Map(c,mastergrids,d) # NB the capital "M" in "Map" i.e. not "map"
x <- c("long","lat","habitat","oldprob","probnew")
mastergrids3 <- lapply(mastergrids2, setNames, x)
mastergrids4 <- lapply(mastergrids3,as.data.frame)


occupy <- function(df){
  occ <- rbinom(nrow(df),size=1,prob=df$probnew)
  cbind(df,occ)
}
occupiedlandscapes <- map(mastergrids4,occupy)


# Check it hasn't gone horribly wrong  
#checks <- as.data.frame(matrix(ncol = 1,nrow = 0))
#idiotcheck <- function(df){
  #m <- mean(df$occ)
  #rbind(checks,m)}
#c <- lapply(occupiedlandscapes, idiotcheck)


#Simulate detection histories
makedetections <- function(df){
  det <- NULL
  for (i in 1:K){
    det <- cbind(det, ifelse(df$habitat==1, rbinom(dim(df)[1],1,prob = df$occ * det.prob.A), rbinom(dim(df)[1],1,prob = df$occ * det.prob.B)))
  }
  det<- as.data.frame(det)
  varname <- "S"  # Select prefix for column names
  names(det)[1:ncol(det)] <- unlist(mapply(function(x,y) paste(x, seq(1,y), sep=""), varname, K)) # Rename columns to S1, S2 etc.
  df <- cbind(df,det) # Recombine detection history with true probabilities 
  # Add site ID numbers
  site <- seq(1:nrow(df))
  df <- cbind(site,df)
}
  
detectedlandscapes <- map(occupiedlandscapes,makedetections)
  
  
# Create unmarked frames for the models
masterunmarked <- list()
makeunmarked <- function(df){
  habitatcov <- df %>% select(habitat)
  det <- df %>% select(starts_with("S",ignore.case = F))
  um_grid <- unmarkedFrameOccu(y=det,siteCovs = habitatcov)
  iteration <- paste("um_grid",mode(df$oldprob),sep="")
  masterunmarked[[iteration]] <- um_grid
}
masterunmarked <- map(detectedlandscapes,makeunmarked)


# Define model function
mod_fun <- function(df){
  occu(~ 1 ~habitat, linkPsi="logit",data=df)
}
# Run model on every simulated landscape
mastermodels <- map(masterunmarked, mod_fun)


# Extract summary for each model
summaries <- list()
extractsummary <- function(list){
  sum <- summary(list)
  summaries <- append(summaries,sum)
}  
summaries <- map(mastermodels,extractsummary)  
  
# Convert list of summaries into a dataframe
notalist <- as.data.frame(unlist(summaries))
notalist$identifier <- row.names((notalist))
colnames(notalist) <- c("value","identifier")

results <- notalist
#results2 <-results %>% gsub(".state.","_state_") %>% gsub(".det.","_det_")

reverse_chars <- function(string)
{
  # split string by characters
  string_split = strsplit(string, split = "")
  # reverse order
  rev_order = nchar(string):1
  # reversed characters
  reversed_chars = string_split[[1]][rev_order]
  # collapse reversed characters
  paste(reversed_chars, collapse = "")
} 

results$identifier_rev <- sapply(results$identifier,reverse_chars)
  
results3 <- results %>% cSplit(splitCols = "identifier_rev",sep=".",direction="wide",drop=F)
results3$identifier_rev_5 <- paste(results3$identifier_rev_3,results3$identifier_rev_4,sep=".")

results3$identifier_rev_1 <- as.character(results3$identifier_rev_1)
results3$identifier_rev_2 <- as.character(results3$identifier_rev_2)
results3$identifier_rev_3 <- as.character(results3$identifier_rev_3)
results3$identifier_rev_4 <- as.character(results3$identifier_rev_4)
results3$identifier_rev_5 <- as.character(results3$identifier_rev_5)


results3$model <- sapply(results3$identifier_rev_5,reverse_chars)
results3$type <- sapply(results3$identifier_rev_2,reverse_chars)
results3$variable <- sapply(results3$identifier_rev_1,reverse_chars)

results3 <- results3 %>% select("value","identifier","model","type","variable")
results3 <- results3 %>% mutate(model=str_replace(model,"AN.",""))
results3$occB <- results3$model
results3 <- results3 %>% mutate(occB=str_replace(model,"grid",""))
results3$occB <- as.numeric(results3$occB)
results3$variable <- as.factor(results3$variable)
results3$type <- as.factor(results3$type)
results3$model <- as.factor(results3$model)

# Subset into results for occupancy (state) and detection
results.state <- results3 %>% filter(type=="state")
results.det <- results3 %>% filter(type=="det")

# Calculate useful numbers
est <- c("Estimate1","Estimate2","SE1","SE2","P(>|z|)2")
state.estimates <- results.state[results.state$variable %in% est,]

est2 <- c("Estimate","SE","P(>|z|)")
det.estimates <- results.det[results.det$variable %in% est2,]

detEst <- det.estimates %>% filter(variable=="Estimate")
detSE <- det.estimates %>% filter(variable=="SE")
detPval <- det.estimates %>% filter(variable=="P(>|z|)")

detEst <- detEst %>% pivot_wider(names_from = "variable", values_from="value")
detSE <- detSE %>% pivot_wider(names_from = "variable", values_from="value")
detPval <- detPval %>% pivot_wider(names_from = "variable", values_from="value")

det.estimates2 <- merge(detEst,detSE,by="occB",all=T,sort=T)
det.estimates2 <- merge(det.estimates2,detPval,by="occB",all=T,sort=T,no.dups=T)
det.estimates3 <- det.estimates2 %>% select("occB","Estimate","SE","P(>|z|)")


Est1 <- state.estimates %>% filter(variable=="Estimate1")
Est2 <- state.estimates %>% filter(variable=="Estimate2")
SE1 <- state.estimates %>% filter(variable=="SE1")
SE2 <- state.estimates %>% filter(variable=="SE2")
Pval <- state.estimates %>% filter(variable=="P(>|z|)2")
Est1 <-Est1 %>% pivot_wider(names_from = "variable", values_from="value")
Est2 <-Est2 %>% pivot_wider(names_from = "variable", values_from="value")
SE1 <-SE1 %>% pivot_wider(names_from = "variable", values_from="value")
SE2 <-SE2 %>% pivot_wider(names_from = "variable", values_from="value")
Pval <-Pval %>% pivot_wider(names_from = "variable", values_from="value")

state.estimates2 <- merge(Est1,Est2,by="occB",all=T,sort=T)
state.estimates2 <- merge(state.estimates2,SE1,by="occB",all=T,sort=T)
state.estimates2 <- merge(state.estimates2,SE2,by="occB",all=T,sort=T,no.dups = T)
state.estimates2 <- merge(state.estimates2,Pval,by="occB",all=T,sort=T,no.dups = T)
state.estimates3 <- state.estimates2 %>% select("occB","Estimate1","Estimate2","SE1","SE2","P(>|z|)2")

state.estimates3$estoccA <- state.estimates3$Estimate1
state.estimates3$estoccB <- state.estimates3$Estimate1 + state.estimates3$Estimate2
state.estimates3$estoccAupper <- state.estimates3$Estimate1 + state.estimates3$SE1
state.estimates3$estoccAlower <- state.estimates3$Estimate1 - state.estimates3$SE1
state.estimates3$estoccBupper <- state.estimates3$Estimate1 + state.estimates3$Estimate2 + state.estimates3$SE2
state.estimates3$estoccBlower <- state.estimates3$Estimate1 + state.estimates3$Estimate2 - state.estimates3$SE2

state.estimates3$estoccAreal <- inv.logit(state.estimates3$estoccA)
state.estimates3$estoccAupperreal <- inv.logit(state.estimates3$estoccAupper)
state.estimates3$estoccAlowerreal <- inv.logit(state.estimates3$estoccAlower)

state.estimates3$estoccBreal <- inv.logit(state.estimates3$estoccB)
state.estimates3$estoccBupperreal <- inv.logit(state.estimates3$estoccBupper)
state.estimates3$estoccBlowerreal <- inv.logit(state.estimates3$estoccBlower)

run <- paste('run:',run,sep='')
allstateresults[[run]] <- state.estimates3
alldetresults[[run]] <- det.estimates3
}

# Transform results list into a useful dataframe
stateresultsdf <- bind_rows(allstateresults, .id = "column_label")
colnames(stateresultsdf)[7] <- "Pvalue"

detresultsdf <- bind_rows(alldetresults, .id = "column_label")
colnames(detresultsdf)[5] <- "Pvalue"

save(stateresultsdf, file="power_analysis_state_results_K7_det01.Rdata") # Save state results to working directory
save(detresultsdf, file="power_analysis_det_results_K7_det01.Rdata") # Save detection results to working directory 

## End of simulation
