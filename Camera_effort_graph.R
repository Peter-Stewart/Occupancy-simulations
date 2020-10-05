#Packages
library(ggplot2)

# Parameters
C <- 25 # Number of cameras
K <- seq(from=2,to=40,by=1) # Number of nights at each site

N <- 75*C # Total camera nights available
S <- N/K # Number of sites surveyed

effort <- as.data.frame(cbind(C,K,N,S))

# Graph
p1 <- ggplot(effort,aes(x=K,y=S)) + geom_line() +
  theme_classic() +
  xlab("Number of nights at each site (K)") +
  ylab("Total sites surveyed") +
  geom_vline(xintercept = 7,lty=2) +
  geom_vline(xintercept = 14,lty=2) +
  geom_vline(xintercept = 21,lty=2) +
  theme(text = element_text(size=15))
  
p1
