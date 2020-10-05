library(dplyr)
library(ggplot2)
library(ggridges)
library(cowplot)
library(boot)

# Det = 0.5
K7det05state <- get(load("power_analysis_state_results_K7_det05.Rdata"))
K14det05state <- get(load("power_analysis_state_results_K14_det05.Rdata"))
K21det05state <- get(load("power_analysis_state_results_K21_det05.Rdata"))

K7det05det <- get(load("power_analysis_det_results_K7_det05.Rdata"))
K14det05det <- get(load("power_analysis_det_results_K14_det05.Rdata"))
K21det05det <- get(load("power_analysis_det_results_K21_det05.Rdata"))

# Det = 0.1
K7det01state <- get(load("power_analysis_state_results_K7_det01.Rdata"))
K14det01state <- get(load("power_analysis_state_results_K14_det01.Rdata"))
K21det01state <- get(load("power_analysis_state_results_K21_det01.Rdata"))

K7det01det <- get(load("power_analysis_det_results_K7_det01.Rdata"))
K14det01det <- get(load("power_analysis_det_results_K14_det01.Rdata"))
K21det01det <- get(load("power_analysis_det_results_K21_det01.Rdata"))

# Det = 0.3
K7det03state <- get(load("power_analysis_state_results_K7_det03.Rdata"))
K14det03state <- get(load("power_analysis_state_results_K14_det03.Rdata"))
K21det03state <- get(load("power_analysis_state_results_K21_det03.Rdata"))

K7det03det <- get(load("power_analysis_det_results_K7_det03.Rdata"))
K14det03det <- get(load("power_analysis_det_results_K14_det03.Rdata"))
K21det03det <- get(load("power_analysis_det_results_K21_det03.Rdata"))
# 
K7red <- K7 %>% filter(occB==c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))
K14red <- K14 %>% filter(occB==c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))
K21red <- K21 %>% filter(occB==c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))



p1a <- ggplot(K7red, aes(x=Pvalue,y=as.factor(occB), fill = as.factor(occB))) +
  geom_density_ridges(stat="binline",binwidth=0.05,alpha=0.5,draw_baseline=F) +
  theme_ridges() + theme(legend.position = "none") +geom_vline(xintercept=0) + 
  geom_vline(xintercept = 0.05,lty=2) + 
  geom_vline(xintercept = 0.1,lty=3) +
  xlab("P value, n=1000 simulations") +
  ylab("True Psi in habitat B") +
  labs(title = "K = 7")
p1a

p1b <- ggplot(K14red, aes(x=Pvalue,y=as.factor(occB), fill = as.factor(occB))) +
  geom_density_ridges(stat="binline",binwidth=0.05,alpha=0.5) +
  theme_ridges() + theme(legend.position = "none") +geom_vline(xintercept=0) + 
  geom_vline(xintercept = 0.05,lty=2) + 
  geom_vline(xintercept = 0.1,lty=3) +
  xlab("P value, n=1000 simulations") +
  ylab("True Psi in habitat B") +
  labs(title = "K = 14")
p1b

p1c <- ggplot(K21red, aes(x=Pvalue,y=as.factor(occB), fill = as.factor(occB))) +
  geom_density_ridges(stat="binline",binwidth=0.05,alpha=0.5) +
  theme_ridges() + theme(legend.position = "none") +geom_vline(xintercept=0) + 
  geom_vline(xintercept = 0.05,lty=2) + 
  geom_vline(xintercept = 0.1,lty=3) +
  xlab("P value, n=1000 simulations") +
  ylab("True Psi in habitat B") +
  labs(title = "K = 21")
p1c



# Summary stats on the results
meanest7det05state <- aggregate(K7det05state$estoccBreal, by = list(Category=K7det05state$occB), FUN = mean, na.rm=T)
colnames(meanest7det05state) <- c("occB","mean")
meansd7det05state <- aggregate(K7det05state$estoccBreal, by = list(Category=K7det05state$occB), FUN = sd, na.rm=T)
colnames(meansd7det05state) <- c("occB","sd")
summaryresults7det05state <- merge(meanest7det05state,meansd7det05state,by="occB")

meanpval7det05state <- aggregate(K7det05state$Pvalue, by = list(Category=K7det05state$occB), FUN = median, na.rm=T)
meanpvalmad7det05state <- aggregate(K7det05state$Pvalue, by = list(Category=K7det05state$occB), FUN = mad, na.rm=T)
colnames(meanpval7det05state) <- c("occB","median")
colnames(meanpvalmad7det05state) <- c("occB","sd")
summarypvals7det05state <- merge(meanpval7det05state,meanpvalmad7det05state,by="occB")

meanest14det05state <- aggregate(K14det05state$estoccBreal, by = list(Category=K14det05state$occB), FUN = mean, na.rm=T)
colnames(meanest14det05state) <- c("occB","mean")
meansd14det05state <- aggregate(K14det05state$estoccBreal, by = list(Category=K14det05state$occB), FUN = sd, na.rm=T)
colnames(meansd14det05state) <- c("occB","sd")
summaryresults14det05state <- merge(meanest14det05state,meansd14det05state,by="occB")

meanpval14det05state <- aggregate(K14det05state$Pvalue, by = list(Category=K14det05state$occB), FUN = median, na.rm=T)
meanpvalmad14det05state <- aggregate(K14det05state$Pvalue, by = list(Category=K14det05state$occB), FUN = mad, na.rm=T)
colnames(meanpval14det05state) <- c("occB","median")
colnames(meanpvalmad14det05state) <- c("occB","sd")
summarypvals14det05state <- merge(meanpval14det05state,meanpvalmad14det05state,by="occB")

meanest21det05state <- aggregate(K21det05state$estoccBreal, by = list(Category=K21det05state$occB), FUN = mean, na.rm=T)
colnames(meanest21det05state) <- c("occB","mean")
meansd21det05state <- aggregate(K21det05state$estoccBreal, by = list(Category=K21det05state$occB), FUN = sd, na.rm=T)
colnames(meansd21det05state) <- c("occB","sd")
summaryresults21det05state <- merge(meanest21det05state,meansd21det05state,by="occB")

meanpval21det05state <- aggregate(K21det05state$Pvalue, by = list(Category=K21det05state$occB), FUN = median, na.rm=T)
meanpvalmad21det05state <- aggregate(K21det05state$Pvalue, by = list(Category=K21det05state$occB), FUN = mad, na.rm=T)
colnames(meanpval21det05state) <- c("occB","median")
colnames(meanpvalmad21det05state) <- c("occB","sd")
summarypvals21det05state <- merge(meanpval21det05state,meanpvalmad21det05state,by="occB")

meanest7det01state <- aggregate(K7det01state$estoccBreal, by = list(Category=K7det01state$occB), FUN = mean, na.rm=T)
colnames(meanest7det01state) <- c("occB","mean")
meansd7det01state <- aggregate(K7det01state$estoccBreal, by = list(Category=K7det01state$occB), FUN = sd, na.rm=T)
colnames(meansd7det01state) <- c("occB","sd")
summaryresults7det01state <- merge(meanest7det01state,meansd7det01state,by="occB")

meanpval7det01state <- aggregate(K7det01state$Pvalue, by = list(Category=K7det01state$occB), FUN = median, na.rm=T)
meanpvalmad7det01state <- aggregate(K7det01state$Pvalue, by = list(Category=K7det01state$occB), FUN = mad, na.rm=T)
colnames(meanpval7det01state) <- c("occB","median")
colnames(meanpvalmad7det01state) <- c("occB","sd")
summarypvals7det01state <- merge(meanpval7det01state,meanpvalmad7det01state,by="occB")

meanest14det01state <- aggregate(K14det01state$estoccBreal, by = list(Category=K14det01state$occB), FUN = mean, na.rm=T)
colnames(meanest14det01state) <- c("occB","mean")
meansd14det01state <- aggregate(K14det01state$estoccBreal, by = list(Category=K14det01state$occB), FUN = sd, na.rm=T)
colnames(meansd14det01state) <- c("occB","sd")
summaryresults14det01state <- merge(meanest14det01state,meansd14det01state,by="occB")

meanpval14det01state <- aggregate(K14det01state$Pvalue, by = list(Category=K14det01state$occB), FUN = median, na.rm=T)
meanpvalmad14det01state <- aggregate(K14det01state$Pvalue, by = list(Category=K14det01state$occB), FUN = mad, na.rm=T)
colnames(meanpval14det01state) <- c("occB","median")
colnames(meanpvalmad14det01state) <- c("occB","sd")
summarypvals14det01state <- merge(meanpval14det01state,meanpvalmad14det01state,by="occB")

meanest21det01state <- aggregate(K21det01state$estoccBreal, by = list(Category=K21det01state$occB), FUN = mean, na.rm=T)
colnames(meanest21det01state) <- c("occB","mean")
meansd21det01state <- aggregate(K21det01state$estoccBreal, by = list(Category=K21det01state$occB), FUN = sd, na.rm=T)
colnames(meansd21det01state) <- c("occB","sd")
summaryresults21det01state <- merge(meanest21det01state,meansd21det01state,by="occB")

meanpval21det01state <- aggregate(K21det01state$Pvalue, by = list(Category=K21det01state$occB), FUN = median, na.rm=T)
meanpvalmad21det01state <- aggregate(K21det01state$Pvalue, by = list(Category=K21det01state$occB), FUN = mad, na.rm=T)
colnames(meanpval21det01state) <- c("occB","median")
colnames(meanpvalmad21det01state) <- c("occB","sd")
summarypvals21det01state <- merge(meanpval21det01state,meanpvalmad21det01state,by="occB")

meanest7det03state <- aggregate(K7det03state$estoccBreal, by = list(Category=K7det03state$occB), FUN = mean, na.rm=T)
colnames(meanest7det03state) <- c("occB","mean")
meansd7det03state <- aggregate(K7det03state$estoccBreal, by = list(Category=K7det03state$occB), FUN = sd, na.rm=T)
colnames(meansd7det03state) <- c("occB","sd")
summaryresults7det03state <- merge(meanest7det03state,meansd7det03state,by="occB")

meanpval7det03state <- aggregate(K7det03state$Pvalue, by = list(Category=K7det03state$occB), FUN = median, na.rm=T)
meanpvalmad7det03state <- aggregate(K7det03state$Pvalue, by = list(Category=K7det03state$occB), FUN = mad, na.rm=T)
colnames(meanpval7det03state) <- c("occB","median")
colnames(meanpvalmad7det03state) <- c("occB","sd")
summarypvals7det03state <- merge(meanpval7det03state,meanpvalmad7det03state,by="occB")

meanest14det03state <- aggregate(K14det03state$estoccBreal, by = list(Category=K14det03state$occB), FUN = mean, na.rm=T)
colnames(meanest14det03state) <- c("occB","mean")
meansd14det03state <- aggregate(K14det03state$estoccBreal, by = list(Category=K14det03state$occB), FUN = sd, na.rm=T)
colnames(meansd14det03state) <- c("occB","sd")
summaryresults14det03state <- merge(meanest14det03state,meansd14det03state,by="occB")

meanpval14det03state <- aggregate(K14det03state$Pvalue, by = list(Category=K14det03state$occB), FUN = median, na.rm=T)
meanpvalmad14det03state <- aggregate(K14det03state$Pvalue, by = list(Category=K14det03state$occB), FUN = mad, na.rm=T)
colnames(meanpval14det03state) <- c("occB","median")
colnames(meanpvalmad14det03state) <- c("occB","sd")
summarypvals14det03state <- merge(meanpval14det03state,meanpvalmad14det03state,by="occB")

meanest21det03state <- aggregate(K21det03state$estoccBreal, by = list(Category=K21det03state$occB), FUN = mean, na.rm=T)
colnames(meanest21det03state) <- c("occB","mean")
meansd21det03state <- aggregate(K21det03state$estoccBreal, by = list(Category=K21det03state$occB), FUN = sd, na.rm=T)
colnames(meansd21det03state) <- c("occB","sd")
summaryresults21det03state <- merge(meanest21det03state,meansd21det03state,by="occB")

meanpval21det03state <- aggregate(K21det03state$Pvalue, by = list(Category=K21det03state$occB), FUN = median, na.rm=T)
meanpvalmad21det03state <- aggregate(K21det03state$Pvalue, by = list(Category=K21det03state$occB), FUN = mad, na.rm=T)
colnames(meanpval21det03state) <- c("occB","median")
colnames(meanpvalmad21det03state) <- c("occB","sd")
summarypvals21det03state <- merge(meanpval21det03state,meanpvalmad21det03state,by="occB")

# Plot p value summaries
p2a <- ggplot(summarypvals7, aes(x=occB,y=median)) + geom_point() + ylim(-0.25,1) +
  geom_errorbar(aes(x=occB,ymax = median+sd, ymin=median-sd)) + theme_classic() + geom_abline(intercept=0,slope=0) +
  geom_hline(yintercept = 0.05, lty=3) +
  geom_vline(xintercept=0.5, lty=2) + ylab("Pvalue +/- MAD, n=1000 simulations") +
  labs(title = "K = 7") +
  xlab("True Psi in habitat B")
p2a

p2b <- ggplot(summarypvals14, aes(x=occB,y=median)) + geom_point() + ylim(-0.25,1) +
  geom_errorbar(aes(x=occB,ymax = median+sd, ymin=median-sd)) + theme_classic() + geom_abline(intercept=0,slope=0) +
  geom_hline(yintercept = 0.05, lty=3) +
  geom_vline(xintercept=0.5, lty=2) + ylab("Pvalue +/- MAD, n=1000 simulations") +
  labs(title = "K = 14") +
  xlab("True Psi in habitat B")
p2b

p2c <- ggplot(summarypvals21, aes(x=occB,y=median)) + geom_point() + ylim(-0.25,1) +
  geom_errorbar(aes(x=occB,ymax = median+sd, ymin=median-sd)) + theme_classic() + geom_abline(intercept=0,slope=0) +
  geom_hline(yintercept = 0.05, lty=3) +
  geom_vline(xintercept=0.5, lty=2) + ylab("Pvalue +/- MAD, n=1000 simulations") +
  labs(title = "K = 21") +
  xlab("True Psi in habitat B")
p2c


p2d <- ggplot(summarypvals7det01state, aes(x=occB,y=median)) + geom_point() + ylim(-0.25,1) +
  geom_errorbar(aes(x=occB,ymax = median+sd, ymin=median-sd)) + theme_classic() + geom_abline(intercept=0,slope=0) +
  geom_hline(yintercept = 0.05, lty=3) +
  geom_vline(xintercept=0.5, lty=2) + ylab("Pvalue +/- MAD, n=1000 simulations") +
  labs(title = "K = 7") +
  xlab("True Psi in habitat B")
p2d

p2e <- ggplot(summarypvals14det01state, aes(x=occB,y=median)) + geom_point() + ylim(-0.25,1) +
  geom_errorbar(aes(x=occB,ymax = median+sd, ymin=median-sd)) + theme_classic() + geom_abline(intercept=0,slope=0) +
  geom_hline(yintercept = 0.05, lty=3) +
  geom_vline(xintercept=0.5, lty=2) + ylab("Pvalue +/- MAD, n=1000 simulations") +
  labs(title = "K = 14") +
  xlab("True Psi in habitat B")
p2e

p2f <- ggplot(summarypvals21det01state, aes(x=occB,y=median)) + geom_point() + ylim(-0.25,1) +
  geom_errorbar(aes(x=occB,ymax = median+sd, ymin=median-sd)) + theme_classic() + geom_abline(intercept=0,slope=0) +
  geom_hline(yintercept = 0.05, lty=3) +
  geom_vline(xintercept=0.5, lty=2) + ylab("Pvalue +/- MAD, n=1000 simulations") +
  labs(title = "K = 21") +
  xlab("True Psi in habitat B")
p2f

p2g <- ggplot(summarypvals7det03state, aes(x=occB,y=median)) + geom_point() + ylim(-0.25,1) +
  geom_errorbar(aes(x=occB,ymax = median+sd, ymin=median-sd)) + theme_classic() + geom_abline(intercept=0,slope=0) +
  geom_hline(yintercept = 0.05, lty=3) +
  geom_vline(xintercept=0.5, lty=2) + ylab("Pvalue +/- MAD, n=1000 simulations") +
  labs(title = "K = 7") +
  xlab("True Psi in habitat B")
p2g

p2h <- ggplot(summarypvals14det03state, aes(x=occB,y=median)) + geom_point() + ylim(-0.25,1) +
  geom_errorbar(aes(x=occB,ymax = median+sd, ymin=median-sd)) + theme_classic() + geom_abline(intercept=0,slope=0) +
  geom_hline(yintercept = 0.05, lty=3) +
  geom_vline(xintercept=0.5, lty=2) + ylab("Pvalue +/- MAD, n=1000 simulations") +
  labs(title = "K = 14") +
  xlab("True Psi in habitat B")
p2h

p2i <- ggplot(summarypvals21det03state, aes(x=occB,y=median)) + geom_point() + ylim(-0.25,1) +
  geom_errorbar(aes(x=occB,ymax = median+sd, ymin=median-sd)) + theme_classic() + geom_abline(intercept=0,slope=0) +
  geom_hline(yintercept = 0.05, lty=3) +
  geom_vline(xintercept=0.5, lty=2) + ylab("Pvalue +/- MAD, n=1000 simulations") +
  labs(title = "K = 21") +
  xlab("True Psi in habitat B")
p2i




######
# Plots of occB estimate vs real occB
# Detection probability = 0.5
plot(estoccBreal ~ occB, data=K7, pch=16, main="K = 7, p = 0.5") + abline(a=0,b=1)
plot(estoccBreal ~ occB, data=K14, pch=16, main="K = 14, p = 0.5") + abline(a=0,b=1)
plot(estoccBreal ~ occB, data=K21, pch=16, main="K = 21, p = 0.5") + abline(a=0,b=1)

# Detection probability = 0.3
plot(estoccBreal ~ occB, data=K7det03state, main="K = 7, p = 0.3") + abline(a=0,b=1)
plot(estoccBreal ~ occB, data=K14det03state, main="K = 14, p = 0.3") + abline(a=0,b=1)
plot(estoccBreal ~ occB, data=K21det03state, main="K = 21, p = 0.3") + abline(a=0,b=1)

# Detection probability = 0.1
plot(estoccBreal ~ occB, data=K7det01state, main="K = 7, p = 0.1") + abline(a=0,b=1)
plot(estoccBreal ~ occB, data=K14det01state, main="K = 14, p = 0.1") + abline(a=0,b=1)
plot(estoccBreal ~ occB, data=K21det01state, main="K = 21, p = 0.1") + abline(a=0,b=1)


# Plots of occA estimate vs real occA
# Detection probability = 0.5
plot(estoccAreal ~ occB, data=K7, pch=16, main="K = 7, p = 0.5") + abline(a=0.5,b=0)
plot(estoccAreal ~ occB, data=K14, pch=16, main="K = 14, p = 0.5") + abline(a=0.5,b=0)
plot(estoccAreal ~ occB, data=K21, pch=16, main="K = 21, p = 0.5") + abline(a=0.5,b=0)

# Detection probability = 0.3
plot(estoccAreal ~ occB, data=K7det03state, main="K = 7, p = 0.3") + abline(a=0.5,b=0)
plot(estoccAreal ~ occB, data=K14det03state, main="K = 14, p = 0.3") + abline(a=0.5,b=0)
plot(estoccAreal ~ occB, data=K21det03state, main="K = 21, p = 0.3") +abline(a=0.5,b=0)

# Detection probability = 0.1
plot(estoccAreal ~ occB, data=K7det01state, main="K = 7, p = 0.1") + abline(a=0.5,b=0)
plot(estoccAreal ~ occB, data=K14det01state, main="K = 14, p = 0.1") + abline(a=0.5,b=0)
plot(estoccAreal ~ occB, data=K21det01state, main="K = 21, p = 0.1") + abline(a=0.5,b=0)


# Plots of standard error 1 (i.e. SE of intercept, habitat A) vs real occB
# Detection probability = 0.5
plot(SE1 ~ occB, data=K7, main="K = 7, p = 0.5") 
plot(SE1 ~ occB, data=K14, main="K = 14, p = 0.5")
plot(SE1 ~ occB, data=K21, main="K = 21, p = 0.5")

# Detection probability = 0.3
plot(SE1 ~ occB, data=K7det03state, main="K = 7, p = 0.3")
plot(SE1 ~ occB, data=K14det03state, main="K = 14, p = 0.3")
plot(SE1 ~ occB, data=K21det03state, main="K = 21, p = 0.3")

# Detection probability = 0.1
plot(SE1 ~ occB, data=K7det01state, main="K = 7, p = 0.1")
plot(SE1 ~ occB, data=K14det01state, main="K = 14, p = 0.1") 
plot(SE1 ~ occB, data=K21det01state, main="K = 21, p = 0.1") 

# Plots of standard error 2 (i.e. SE of the effect, the difference between habitat B and A) vs real occB
par(mfrow=c(3,3))
# Detection probability = 0.5
plot(SE2 ~ occB, data=K7det05state, main="K = 7, p = 0.5") 
plot(SE2 ~ occB, data=K14det05state, main="K = 14, p = 0.5")
plot(SE2 ~ occB, data=K21det05state, main="K = 21, p = 0.5")

# Detection probability = 0.3
plot(SE2 ~ occB, data=K7det03state, main="K = 7, p = 0.3")
plot(SE2 ~ occB, data=K14det03state, main="K = 14, p = 0.3")
plot(SE2 ~ occB, data=K21det03state, main="K = 21, p = 0.3")

# Detection probability = 0.1
plot(SE2 ~ occB, data=K7det01state, main="K = 7, p = 0.1")
plot(SE2 ~ occB, data=K14det01state, main="K = 14, p = 0.1") 
plot(SE2 ~ occB, data=K21det01state, main="K = 21, p = 0.1") 

par(mfrow=c(1,1))
# Plot P value vs. occB
# Detection probability = 0.5
plot(Pvalue ~ occB, data=K7, main="K = 7, p = 0.5") 
plot(Pvalue ~ occB, data=K14, main="K = 14, p = 0.5")
plot(Pvalue ~ occB, data=K21, main="K = 21, p = 0.5")

# Detection probability = 0.3
plot(Pvalue ~ occB, data=K7det03state, main="K = 7, p = 0.3")
plot(Pvalue ~ occB, data=K14det03state, main="K = 14, p = 0.3")
plot(Pvalue ~ occB, data=K21det03state, main="K = 21, p = 0.3")

# Detection probability = 0.1
plot(Pvalue ~ occB, data=K7det01state, main="K = 7, p = 0.1")
plot(Pvalue ~ occB, data=K14det01state, main="K = 14, p = 0.1") 
plot(Pvalue ~ occB, data=K21det01state, main="K = 21, p = 0.1") 

### Need to plot NA values for each variable vs. occB to see if there are patterns in model failure
# Could change NA standard errors to a negative number? 
# Or aggregate by counting number of NA values

# Plot p values including NA values to look at patterns of model failure
K7det05statepvalNA <- K7det05state %>% filter(is.na(Pvalue))
K7det05statepvalNA[is.na(K7det05statepvalNA)] <- 1
K7det05statepvalNA <- aggregate(K7det05statepvalNA$estoccBreal, by = list(Category=K7det05statepvalNA$occB), FUN = sum)
colnames(K7det05statepvalNA) <- c("occB","numNA")
K7det05statepvalNA2 <- merge(summarypvals7det05state,K7det05statepvalNA,by="occB",all = T)

p4a <- ggplot(K7det05statepvalNA2) +
  geom_col(aes(x=occB,y=numNA/400),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=median)) +
  geom_errorbar(aes(x=occB,ymax=median+sd,ymin=median-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "Number of NA values")) +
  ylab("Median Pvalue +/- MAD, n=1000 simulations") +
  theme_classic()
p4a

K14det05statepvalNA <- K14det05state %>% filter(is.na(Pvalue))
K14det05statepvalNA[is.na(K14det05statepvalNA)] <- 1
K14det05statepvalNA <- aggregate(K14det05statepvalNA$estoccBreal, by = list(Category=K14det05statepvalNA$occB), FUN = sum)
colnames(K14det05statepvalNA) <- c("occB","numNA")
K14det05statepvalNA2 <- merge(summarypvals14det05state,K14det05statepvalNA,by="occB",all = T)

p4b <- ggplot(K14det05statepvalNA2) +
  geom_col(aes(x=occB,y=numNA/400),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=median)) +
  geom_errorbar(aes(x=occB,ymax=median+sd,ymin=median-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "Number of NA values")) +
  ylab("Median Pvalue +/- MAD, n=1000 simulations") +
  theme_classic()
p4b

K21det05statepvalNA <- K21det05state %>% filter(is.na(Pvalue))
K21det05statepvalNA[is.na(K21det05statepvalNA)] <- 1
K21det05statepvalNA <- aggregate(K21det05statepvalNA$estoccBreal, by = list(Category=K21det05statepvalNA$occB), FUN = sum)
colnames(K21det05statepvalNA) <- c("occB","numNA")
K21det05statepvalNA2 <- merge(summarypvals21det05state,K21det05statepvalNA,by="occB",all = T)

p4c <- ggplot(K21det05statepvalNA2) +
  geom_col(aes(x=occB,y=numNA/400),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=median)) +
  geom_errorbar(aes(x=occB,ymax=median+sd,ymin=median-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "Number of NA values")) +
  ylab("Median Pvalue +/- MAD, n=1000 simulations") +
  theme_classic()
p4c


K7det01statepvalNA <- K7det01state %>% filter(is.na(Pvalue))
K7det01statepvalNA[is.na(K7det01statepvalNA)] <- 1
K7det01statepvalNA <- aggregate(K7det01statepvalNA$estoccBreal, by = list(Category=K7det01statepvalNA$occB), FUN = sum)
colnames(K7det01statepvalNA) <- c("occB","numNA")
K7det01statepvalNA2 <- merge(summarypvals7det01state,K7det01statepvalNA,by="occB",all = T)

p4d <- ggplot(K7det01statepvalNA2) +
  geom_col(aes(x=occB,y=numNA/400),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=median)) +
  geom_errorbar(aes(x=occB,ymax=median+sd,ymin=median-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "Number of NA values")) +
  ylab("Median Pvalue +/- MAD, n=1000 simulations") +
  theme_classic()
p4d

K14det01statepvalNA <- K14det01state %>% filter(is.na(Pvalue))
K14det01statepvalNA[is.na(K14det01statepvalNA)] <- 1
K14det01statepvalNA <- aggregate(K14det01statepvalNA$estoccBreal, by = list(Category=K14det01statepvalNA$occB), FUN = sum)
colnames(K14det01statepvalNA) <- c("occB","numNA")
K14det01statepvalNA2 <- merge(summarypvals14det01state,K14det01statepvalNA,by="occB",all = T)

p4e <- ggplot(K14det01statepvalNA2) +
  geom_col(aes(x=occB,y=numNA/800),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=median)) +
  geom_errorbar(aes(x=occB,ymax=median+sd,ymin=median-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*800, name = "Number of NA values")) +
  ylab("Median Pvalue +/- MAD, n=1000 simulations") +
  theme_classic()
p4e

K21det01statepvalNA <- K21det01state %>% filter(is.na(Pvalue))
K21det01statepvalNA[is.na(K21det01statepvalNA)] <- 1
K21det01statepvalNA <- aggregate(K21det01statepvalNA$estoccBreal, by = list(Category=K21det01statepvalNA$occB), FUN = sum)
colnames(K21det01statepvalNA) <- c("occB","numNA")
K21det01statepvalNA2 <- merge(summarypvals21det01state,K21det01statepvalNA,by="occB",all = T)

p4f <- ggplot(K21det01statepvalNA2) +
  geom_col(aes(x=occB,y=numNA/800),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=median)) +
  geom_errorbar(aes(x=occB,ymax=median+sd,ymin=median-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*800, name = "Number of NA values")) +
  ylab("Median Pvalue +/- MAD, n=1000 simulations") +
  theme_classic()
p4f


K7det03statepvalNA <- K7det03state %>% filter(is.na(Pvalue))
K7det03statepvalNA[is.na(K7det03statepvalNA)] <- 1
K7det03statepvalNA <- aggregate(K7det03statepvalNA$estoccBreal, by = list(Category=K7det03statepvalNA$occB), FUN = sum)
colnames(K7det03statepvalNA) <- c("occB","numNA")
K7det03statepvalNA2 <- merge(summarypvals7det03state,K7det03statepvalNA,by="occB",all = T)

p4g <- ggplot(K7det03statepvalNA2) +
  geom_col(aes(x=occB,y=numNA/400),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=median)) +
  geom_errorbar(aes(x=occB,ymax=median+sd,ymin=median-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "Number of NA values")) +
  ylab("Median Pvalue +/- MAD, n=1000 simulations") +
  theme_classic()
p4g

K14det03statepvalNA <- K14det03state %>% filter(is.na(Pvalue))
K14det03statepvalNA[is.na(K14det03statepvalNA)] <- 1
K14det03statepvalNA <- aggregate(K14det03statepvalNA$estoccBreal, by = list(Category=K14det03statepvalNA$occB), FUN = sum)
colnames(K14det03statepvalNA) <- c("occB","numNA")
K14det03statepvalNA2 <- merge(summarypvals14det03state,K14det03statepvalNA,by="occB",all = T)

p4h <- ggplot(K14det03statepvalNA2) +
  geom_col(aes(x=occB,y=numNA/400),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=median)) +
  geom_errorbar(aes(x=occB,ymax=median+sd,ymin=median-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "Number of NA values")) +
  ylab("Median Pvalue +/- MAD, n=1000 simulations") +
  theme_classic()
p4h

K21det03statepvalNA <- K21det03state %>% filter(is.na(Pvalue))
K21det03statepvalNA[is.na(K21det03statepvalNA)] <- 1
K21det03statepvalNA <- aggregate(K21det03statepvalNA$estoccBreal, by = list(Category=K21det03statepvalNA$occB), FUN = sum)
colnames(K21det03statepvalNA) <- c("occB","numNA")
K21det03statepvalNA2 <- merge(summarypvals21det03state,K21det03statepvalNA,by="occB",all = T)

p4i <- ggplot(K21det03statepvalNA2) +
  geom_col(aes(x=occB,y=numNA/400),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=median)) +
  geom_errorbar(aes(x=occB,ymax=median+sd,ymin=median-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "Number of NA values")) +
  ylab("Median Pvalue +/- MAD, n=1000 simulations") +
  theme_classic()
p4i

# Plot all these graphs together
grid1 <- plot_grid(p4a,p4b,p4c,p4g,p4h,p4i,p4d,p4e,p4f,nrow=3,ncol=3,
                   labels=c("K=7, p=0.5","K=14, p=0.5","K=21, p=0.5",
                            "K=7, p=0.3","K=14, p=0.3","K=21, p=0.3",
                            "K=7, p=0.1","K=14, p=0.1","K=21, p=0.1"))
grid1



#### Similar stats needed for the other estimates - i.e. plot NA's alongside


### Plots of detection probability
## Plots of raw data
# Estimates
plot(Estimate~occB,data=K7det05det)
plot(Estimate~occB,data=K7det03det)
plot(Estimate~occB,data=K7det01det)

plot(Estimate~occB,data=K14det05det)
plot(Estimate~occB,data=K14det03det)
plot(Estimate~occB,data=K14det01det)

plot(Estimate~occB,data=K21det05det)
plot(Estimate~occB,data=K21det03det)
plot(Estimate~occB,data=K21det01det)

# Standard errors
plot(SE~occB,data=K7det05det)
plot(SE~occB,data=K7det03det)
plot(SE~occB,data=K7det01det)

plot(SE~occB,data=K14det05det)
plot(SE~occB,data=K14det03det)
plot(SE~occB,data=K14det01det)

plot(SE~occB,data=K21det05det)
plot(SE~occB,data=K21det03det)
plot(SE~occB,data=K21det01det)


# Plots for detection probability
# Summary stats on the results
K7det05det$Estimate2 <- inv.logit(K7det05det$Estimate)
K14det05det$Estimate2 <- inv.logit(K14det05det$Estimate)
K21det05det$Estimate2 <- inv.logit(K21det05det$Estimate)

K7det03det$Estimate2 <- inv.logit(K7det03det$Estimate)
K14det03det$Estimate2 <- inv.logit(K14det03det$Estimate)
K21det03det$Estimate2 <- inv.logit(K21det03det$Estimate)

K7det01det$Estimate2 <- inv.logit(K7det01det$Estimate)
K14det01det$Estimate2 <- inv.logit(K14det01det$Estimate)
K21det01det$Estimate2 <- inv.logit(K21det01det$Estimate)


meanest7det05det <- aggregate(K7det05det$Estimate2, by = list(Category=K7det05det$occB), FUN = mean, na.rm=T)
colnames(meanest7det05det) <- c("occB","mean")
meansd7det05det <- aggregate(K7det05det$Estimate2, by = list(Category=K7det05det$occB), FUN = sd, na.rm=T)
colnames(meansd7det05det) <- c("occB","sd")
summaryresults7det05det <- merge(meanest7det05det,meansd7det05det,by="occB")

meanest14det05det <- aggregate(K14det05det$Estimate2, by = list(Category=K14det05det$occB), FUN = mean, na.rm=T)
colnames(meanest14det05det) <- c("occB","mean")
meansd14det05det <- aggregate(K14det05det$Estimate2, by = list(Category=K14det05det$occB), FUN = sd, na.rm=T)
colnames(meansd14det05det) <- c("occB","sd")
summaryresults14det05det <- merge(meanest14det05det,meansd14det05det,by="occB")

meanest21det05det <- aggregate(K21det05det$Estimate2, by = list(Category=K21det05det$occB), FUN = mean, na.rm=T)
colnames(meanest21det05det) <- c("occB","mean")
meansd21det05det <- aggregate(K21det05det$Estimate2, by = list(Category=K21det05det$occB), FUN = sd, na.rm=T)
colnames(meansd21det05det) <- c("occB","sd")
summaryresults21det05det <- merge(meanest21det05det,meansd21det05det,by="occB")

meanest7det01det <- aggregate(K7det01det$Estimate2, by = list(Category=K7det01det$occB), FUN = mean, na.rm=T)
colnames(meanest7det01det) <- c("occB","mean")
meansd7det01det <- aggregate(K7det01det$Estimate2, by = list(Category=K7det01det$occB), FUN = sd, na.rm=T)
colnames(meansd7det01det) <- c("occB","sd")
summaryresults7det01det <- merge(meanest7det01det,meansd7det01det,by="occB")

meanest14det01det <- aggregate(K14det01det$Estimate2, by = list(Category=K14det01det$occB), FUN = mean, na.rm=T)
colnames(meanest14det01det) <- c("occB","mean")
meansd14det01det <- aggregate(K14det01det$Estimate2, by = list(Category=K14det01det$occB), FUN = sd, na.rm=T)
colnames(meansd14det01det) <- c("occB","sd")
summaryresults14det01det <- merge(meanest14det01det,meansd14det01det,by="occB")

meanest21det01det <- aggregate(K21det01det$Estimate2, by = list(Category=K21det01det$occB), FUN = mean, na.rm=T)
colnames(meanest21det01det) <- c("occB","mean")
meansd21det01det <- aggregate(K21det01det$Estimate2, by = list(Category=K21det01det$occB), FUN = sd, na.rm=T)
colnames(meansd21det01det) <- c("occB","sd")
summaryresults21det01det <- merge(meanest21det01det,meansd21det01det,by="occB")

meanest7det03det <- aggregate(K7det03det$Estimate2, by = list(Category=K7det03det$occB), FUN = mean, na.rm=T)
colnames(meanest7det03det) <- c("occB","mean")
meansd7det03det <- aggregate(K7det03det$Estimate2, by = list(Category=K7det03det$occB), FUN = sd, na.rm=T)
colnames(meansd7det03det) <- c("occB","sd")
summaryresults7det03det <- merge(meanest7det03det,meansd7det03det,by="occB")

meanest14det03det <- aggregate(K14det03det$Estimate2, by = list(Category=K14det03det$occB), FUN = mean, na.rm=T)
colnames(meanest14det03det) <- c("occB","mean")
meansd14det03det <- aggregate(K14det03det$Estimate2, by = list(Category=K14det03det$occB), FUN = sd, na.rm=T)
colnames(meansd14det03det) <- c("occB","sd")
summaryresults14det03det <- merge(meanest14det03det,meansd14det03det,by="occB")

meanest21det03det <- aggregate(K21det03det$Estimate2, by = list(Category=K21det03det$occB), FUN = mean, na.rm=T)
colnames(meanest21det03det) <- c("occB","mean")
meansd21det03det <- aggregate(K21det03det$Estimate2, by = list(Category=K21det03det$occB), FUN = sd, na.rm=T)
colnames(meansd21det03det) <- c("occB","sd")
summaryresults21det03det <- merge(meanest21det03det,meansd21det03det,by="occB")



# Plot estimates including NA values to look at patterns of model failure
K7det05detestNA <- K7det05det %>% filter(is.na(Estimate2))
K7det05detestNA[is.na(K7det05detestNA)] <- 1
K7det05detestNA <- aggregate(K7det05detestNA$Estimate2, by = list(Category=K7det05detestNA$occB), FUN = sum)
colnames(K7det05detestNA) <- c("occB","numNA")
K7det05detestNA2 <- merge(summaryresults7det05det,K7det05detestNA,by="occB",all=T)

p6a <- ggplot(K7det05detestNA2) +
  geom_col(aes(x=occB,y=numNA/400),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=mean)) +
  geom_errorbar(aes(x=occB,ymax=mean+sd,ymin=mean-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "Number of NA values")) +
  ylab("Mean Estimate2 +/- SD, n=1000 simulations") +
  theme_classic()
p6a

K14det05detestNA <- K14det05det %>% filter(is.na(Estimate2))
K14det05detestNA[is.na(K14det05detestNA)] <- 1
K14det05detestNA <- aggregate(K14det05detestNA$Estimate2, by = list(Category=K14det05detestNA$occB), FUN = sum)
colnames(K14det05detestNA) <- c("occB","numNA")
K14det05detestNA2 <- merge(summaryresults14det05det,K14det05detestNA,by="occB",all=T)

p6b <- ggplot(K14det05detestNA2) +
  geom_col(aes(x=occB,y=numNA/400),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=mean)) +
  geom_errorbar(aes(x=occB,ymax=mean+sd,ymin=mean-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "Number of NA values")) +
  ylab("Mean Estimate2 +/- SD, n=1000 simulations") +
  theme_classic()
p6b

K21det05detestNA <- K21det05det %>% filter(is.na(Estimate2))
K21det05detestNA[is.na(K21det05detestNA)] <- 1
K21det05detestNA <- aggregate(K21det05detestNA$Estimate2, by = list(Category=K21det05detestNA$occB), FUN = sum)
colnames(K21det05detestNA) <- c("occB","numNA")
K21det05detestNA2 <- merge(summaryresults21det05det,K21det05detestNA,by="occB",all=T)

p6c <- ggplot(K21det05detestNA2) +
  geom_col(aes(x=occB,y=numNA/400),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=mean)) +
  geom_errorbar(aes(x=occB,ymax=mean+sd,ymin=mean-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "Number of NA values")) +
  ylab("Mean Estimate2 +/- SD, n=1000 simulations") +
  theme_classic()
p6c

K7det03detestNA <- K7det03det %>% filter(is.na(Estimate2))
K7det03detestNA[is.na(K7det03detestNA)] <- 1
K7det03detestNA <- aggregate(K7det03detestNA$Estimate2, by = list(Category=K7det03detestNA$occB), FUN = sum)
colnames(K7det03detestNA) <- c("occB","numNA")
K7det03detestNA2 <- merge(summaryresults7det03det,K7det03detestNA,by="occB",all=T)

p6d <- ggplot(K7det03detestNA2) +
  geom_col(aes(x=occB,y=numNA/400),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=mean)) +
  geom_errorbar(aes(x=occB,ymax=mean+sd,ymin=mean-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "Number of NA values")) +
  ylab("Mean Estimate2 +/- SD, n=1000 simulations") +
  theme_classic()
p6d

K14det03detestNA <- K14det03det %>% filter(is.na(Estimate2))
K14det03detestNA[is.na(K14det03detestNA)] <- 1
K14det03detestNA <- aggregate(K14det03detestNA$Estimate2, by = list(Category=K14det03detestNA$occB), FUN = sum)
colnames(K14det03detestNA) <- c("occB","numNA")
K14det03detestNA2 <- merge(summaryresults14det03det,K14det03detestNA,by="occB",all=T)

p6e <- ggplot(K14det03detestNA2) +
  geom_col(aes(x=occB,y=numNA/400),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=mean)) +
  geom_errorbar(aes(x=occB,ymax=mean+sd,ymin=mean-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "Number of NA values")) +
  ylab("Mean Estimate2 +/- SD, n=1000 simulations") +
  theme_classic()
p6e

K21det03detestNA <- K21det03det %>% filter(is.na(Estimate2))
K21det03detestNA[is.na(K21det03detestNA)] <- 1
K21det03detestNA <- aggregate(K21det03detestNA$Estimate2, by = list(Category=K21det03detestNA$occB), FUN = sum)
colnames(K21det03detestNA) <- c("occB","numNA")
K21det03detestNA2 <- merge(summaryresults21det03det,K21det03detestNA,by="occB",all=T)

p6f <- ggplot(K21det03detestNA2) +
  geom_col(aes(x=occB,y=numNA/400),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=mean)) +
  geom_errorbar(aes(x=occB,ymax=mean+sd,ymin=mean-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "Number of NA values")) +
  ylab("Mean Estimate2 +/- SD, n=1000 simulations") +
  theme_classic()
p6f

K7det01detestNA <- K7det01det %>% filter(is.na(Estimate2))
K7det01detestNA[is.na(K7det01detestNA)] <- 1
K7det01detestNA <- aggregate(K7det01detestNA$Estimate2, by = list(Category=K7det01detestNA$occB), FUN = sum)
colnames(K7det01detestNA) <- c("occB","numNA")
K7det01detestNA2 <- merge(summaryresults7det01det,K7det01detestNA,by="occB",all=T)

p6g <- ggplot(K7det01detestNA2) +
  geom_col(aes(x=occB,y=numNA/400),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=mean)) +
  geom_errorbar(aes(x=occB,ymax=mean+sd,ymin=mean-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "Number of NA values")) +
  ylab("Mean Estimate2 +/- SD, n=1000 simulations") +
  theme_classic()
p6g

K14det01detestNA <- K14det01det %>% filter(is.na(Estimate2))
K14det01detestNA[is.na(K14det01detestNA)] <- 1
K14det01detestNA <- aggregate(K14det01detestNA$Estimate2, by = list(Category=K14det01detestNA$occB), FUN = sum)
colnames(K14det01detestNA) <- c("occB","numNA")
K14det01detestNA2 <- merge(summaryresults14det01det,K14det01detestNA,by="occB",all=T)

p6h <- ggplot(K14det01detestNA2) +
  geom_col(aes(x=occB,y=numNA/400),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=mean)) +
  geom_errorbar(aes(x=occB,ymax=mean+sd,ymin=mean-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "Number of NA values")) +
  ylab("Mean Estimate2 +/- SD, n=1000 simulations") +
  theme_classic()
p6h

K21det01detestNA <- K21det01det %>% filter(is.na(Estimate2))
K21det01detestNA[is.na(K21det01detestNA)] <- 1
K21det01detestNA <- aggregate(K21det01detestNA$Estimate2, by = list(Category=K21det01detestNA$occB), FUN = sum)
colnames(K21det01detestNA) <- c("occB","numNA")
K21det01detestNA2 <- merge(summaryresults21det01det,K21det01detestNA,by="occB",all=T)

p6i <- ggplot(K21det01detestNA2) +
  geom_col(aes(x=occB,y=numNA/400),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=mean)) +
  geom_errorbar(aes(x=occB,ymax=mean+sd,ymin=mean-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "Number of NA values")) +
  ylab("Mean Estimate2 +/- SD, n=1000 simulations") +
  theme_classic()
p6i

# Plot all these graphs together
grid1 <- plot_grid(p6a,p6b,p6c,p6d,p6e,p6f,p6g,p6h,p6i,nrow=3,ncol=3,
                   labels=c("K=7, p=0.5","K=14, p=0.5","K=21, p=0.5",
                            "K=7, p=0.3","K=14, p=0.3","K=21, p=0.3",
                            "K=7, p=0.1","K=14, p=0.1","K=21, p=0.1"))
grid1


####
meanse7det05det <- aggregate(K7det05det$SE, by = list(Category=K7det05det$occB), FUN = mean, na.rm=T)
colnames(meanse7det05det) <- c("occB","mean")
meansesd7det05det <- aggregate(K7det05det$SE, by = list(Category=K7det05det$occB), FUN = sd, na.rm=T)
colnames(meansesd7det05det) <- c("occB","sd")
summaryseresults7det05det <- merge(meanse7det05det,meansesd7det05det,by="occB")

meanse14det05det <- aggregate(K14det05det$SE, by = list(Category=K14det05det$occB), FUN = mean, na.rm=T)
colnames(meanse14det05det) <- c("occB","mean")
meansesd14det05det <- aggregate(K14det05det$SE, by = list(Category=K14det05det$occB), FUN = sd, na.rm=T)
colnames(meansesd14det05det) <- c("occB","sd")
summaryseresults14det05det <- merge(meanse14det05det,meansesd14det05det,by="occB")

meanse21det05det <- aggregate(K21det05det$SE, by = list(Category=K21det05det$occB), FUN = mean, na.rm=T)
colnames(meanse21det05det) <- c("occB","mean")
meansesd21det05det <- aggregate(K21det05det$SE, by = list(Category=K21det05det$occB), FUN = sd, na.rm=T)
colnames(meansesd21det05det) <- c("occB","sd")
summaryseresults21det05det <- merge(meanse21det05det,meansesd21det05det,by="occB")

meanse7det01det <- aggregate(K7det01det$SE, by = list(Category=K7det01det$occB), FUN = mean, na.rm=T)
colnames(meanse7det01det) <- c("occB","mean")
meansesd7det01det <- aggregate(K7det01det$SE, by = list(Category=K7det01det$occB), FUN = sd, na.rm=T)
colnames(meansesd7det01det) <- c("occB","sd")
summaryseresults7det01det <- merge(meanse7det01det,meansesd7det01det,by="occB")

meanse14det01det <- aggregate(K14det01det$SE, by = list(Category=K14det01det$occB), FUN = mean, na.rm=T)
colnames(meanse14det01det) <- c("occB","mean")
meansesd14det01det <- aggregate(K14det01det$SE, by = list(Category=K14det01det$occB), FUN = sd, na.rm=T)
colnames(meansesd14det01det) <- c("occB","sd")
summaryseresults14det01det <- merge(meanse14det01det,meansesd14det01det,by="occB")

meanse21det01det <- aggregate(K21det01det$SE, by = list(Category=K21det01det$occB), FUN = mean, na.rm=T)
colnames(meanse21det01det) <- c("occB","mean")
meansesd21det01det <- aggregate(K21det01det$SE, by = list(Category=K21det01det$occB), FUN = sd, na.rm=T)
colnames(meansesd21det01det) <- c("occB","sd")
summaryseresults21det01det <- merge(meanse21det01det,meansesd21det01det,by="occB")

meanse7det03det <- aggregate(K7det03det$SE, by = list(Category=K7det03det$occB), FUN = mean, na.rm=T)
colnames(meanse7det03det) <- c("occB","mean")
meansesd7det03det <- aggregate(K7det03det$SE, by = list(Category=K7det03det$occB), FUN = sd, na.rm=T)
colnames(meansesd7det03det) <- c("occB","sd")
summaryseresults7det03det <- merge(meanse7det03det,meansesd7det03det,by="occB")

meanse14det03det <- aggregate(K14det03det$SE, by = list(Category=K14det03det$occB), FUN = mean, na.rm=T)
colnames(meanse14det03det) <- c("occB","mean")
meansesd14det03det <- aggregate(K14det03det$SE, by = list(Category=K14det03det$occB), FUN = sd, na.rm=T)
colnames(meansesd14det03det) <- c("occB","sd")
summaryseresults14det03det <- merge(meanse14det03det,meansesd14det03det,by="occB")

meanse21det03det <- aggregate(K21det03det$SE, by = list(Category=K21det03det$occB), FUN = mean, na.rm=T)
colnames(meanse21det03det) <- c("occB","mean")
meansesd21det03det <- aggregate(K21det03det$SE, by = list(Category=K21det03det$occB), FUN = sd, na.rm=T)
colnames(meansesd21det03det) <- c("occB","sd")
summaryseresults21det03det <- merge(meanse21det03det,meansesd21det03det,by="occB")

K7det05detseNA <- K7det05det %>% filter(is.na(SE))
K7det05detseNA[is.na(K7det05detseNA)] <- 1
K7det05detseNA <- aggregate(K7det05detseNA$SE, by = list(Category=K7det05detseNA$occB), FUN = sum)
colnames(K7det05detseNA) <- c("occB","numNA")
K7det05detseNA2 <- merge(summaryresults21det05det,K7det05detseNA,by="occB",all=T)

K7det03detseNA <- K7det03det %>% filter(is.na(SE))
K7det03detseNA[is.na(K7det03detseNA)] <- 1
K7det03detseNA <- aggregate(K7det03detseNA$SE, by = list(Category=K7det03detseNA$occB), FUN = sum)
colnames(K7det03detseNA) <- c("occB","numNA")
K7det03detseNA2 <- merge(summaryresults21det03det,K7det03detseNA,by="occB",all=T)

K7det01detseNA <- K7det01det %>% filter(is.na(SE))
K7det01detseNA[is.na(K7det01detseNA)] <- 1
K7det01detseNA <- aggregate(K7det01detseNA$SE, by = list(Category=K7det01detseNA$occB), FUN = sum)
colnames(K7det01detseNA) <- c("occB","numNA")
K7det01detseNA2 <- merge(summaryresults21det01det,K7det01detseNA,by="occB",all=T)

K14det05detseNA <- K14det05det %>% filter(is.na(SE))
K14det05detseNA[is.na(K14det05detseNA)] <- 1
K14det05detseNA <- aggregate(K14det05detseNA$SE, by = list(Category=K14det05detseNA$occB), FUN = sum)
colnames(K14det05detseNA) <- c("occB","numNA")
K14det05detseNA2 <- merge(summaryresults21det05det,K14det05detseNA,by="occB",all=T)

K14det03detseNA <- K14det03det %>% filter(is.na(SE))
K14det03detseNA[is.na(K14det03detseNA)] <- 1
K14det03detseNA <- aggregate(K14det03detseNA$SE, by = list(Category=K14det03detseNA$occB), FUN = sum)
colnames(K14det03detseNA) <- c("occB","numNA")
K14det03detseNA2 <- merge(summaryresults21det03det,K14det03detseNA,by="occB",all=T)

K14det01detseNA <- K14det01det %>% filter(is.na(SE))
K14det01detseNA[is.na(K14det01detseNA)] <- 1
K14det01detseNA <- aggregate(K14det01detseNA$SE, by = list(Category=K14det01detseNA$occB), FUN = sum)
colnames(K14det01detseNA) <- c("occB","numNA")
K14det01detseNA2 <- merge(summaryresults21det01det,K14det01detseNA,by="occB",all=T)

K21det05detseNA <- K21det05det %>% filter(is.na(SE))
K21det05detseNA[is.na(K21det05detseNA)] <- 1
K21det05detseNA <- aggregate(K21det05detseNA$SE, by = list(Category=K21det05detseNA$occB), FUN = sum)
colnames(K21det05detseNA) <- c("occB","numNA")
K21det05detseNA2 <- merge(summaryresults21det05det,K21det05detseNA,by="occB",all=T)

K21det03detseNA <- K21det03det %>% filter(is.na(SE))
K21det03detseNA[is.na(K21det03detseNA)] <- 1
K21det03detseNA <- aggregate(K21det03detseNA$SE, by = list(Category=K21det03detseNA$occB), FUN = sum)
colnames(K21det03detseNA) <- c("occB","numNA")
K21det03detseNA2 <- merge(summaryresults21det03det,K21det03detseNA,by="occB",all=T)

K21det01detseNA <- K21det01det %>% filter(is.na(SE))
K21det01detseNA[is.na(K21det01detseNA)] <- 1
K21det01detseNA <- aggregate(K21det01detseNA$SE, by = list(Category=K21det01detseNA$occB), FUN = sum)
colnames(K21det01detseNA) <- c("occB","numNA")
K21det01detseNA2 <- merge(summaryresults21det01det,K21det01detseNA,by="occB",all=T)


p7a <- ggplot(K7det05detseNA2) +
  geom_col(aes(x=occB,y=numNA/400),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=mean)) +
  geom_errorbar(aes(x=occB,ymax=mean+sd,ymin=mean-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "Number of NA values")) +
  ylab("Mean SE of Estimate +/- SD, n=1000 simulations") +
  theme_classic()
p7a

p7b <- ggplot(K14det05detseNA2) +
  geom_col(aes(x=occB,y=numNA/400),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=mean)) +
  geom_errorbar(aes(x=occB,ymax=mean+sd,ymin=mean-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "Number of NA values")) +
  ylab("Mean SE of Estimate +/- SD, n=1000 simulations") +
  theme_classic()
p7b

p7c <<- ggplot(K21det05detseNA2) +
  geom_col(aes(x=occB,y=numNA/400),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=mean)) +
  geom_errorbar(aes(x=occB,ymax=mean+sd,ymin=mean-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "Number of NA values")) +
  ylab("Mean SE of Estimate +/- SD, n=1000 simulations") +
  theme_classic()
p7c

p7d <- ggplot(K7det03detseNA2) +
  geom_col(aes(x=occB,y=numNA/400),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=mean)) +
  geom_errorbar(aes(x=occB,ymax=mean+sd,ymin=mean-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "Number of NA values")) +
  ylab("Mean SE of Estimate +/- SD, n=1000 simulations") +
  theme_classic()
p7d

p7e <- ggplot(K14det03detseNA2) +
  geom_col(aes(x=occB,y=numNA/100),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=mean)) +
  geom_errorbar(aes(x=occB,ymax=mean+sd,ymin=mean-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*100, name = "Number of NA values")) +
  ylab("Mean SE of Estimate +/- SD, n=1000 simulations") +
  theme_classic()
p7e

p7f <- ggplot(K21det03detseNA2) +
  geom_col(aes(x=occB,y=numNA/400),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=mean)) +
  geom_errorbar(aes(x=occB,ymax=mean+sd,ymin=mean-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "Number of NA values")) +
  ylab("Mean SE of Estimate +/- SD, n=1000 simulations") +
  theme_classic()
p7f

p7g <- ggplot(K7det01detseNA2) +
  geom_col(aes(x=occB,y=numNA/400),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=mean)) +
  geom_errorbar(aes(x=occB,ymax=mean+sd,ymin=mean-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "Number of NA values")) +
  ylab("Mean SE of Estimate +/- SD, n=1000 simulations") +
  theme_classic()
p7g

p7h <- ggplot(K14det01detseNA2) +
  geom_col(aes(x=occB,y=numNA/1000),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=mean)) +
  geom_errorbar(aes(x=occB,ymax=mean+sd,ymin=mean-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1000, name = "Number of NA values")) +
  ylab("Mean SE of Estimate +/- SD, n=1000 simulations") +
  theme_classic()
p7h

p7i <- ggplot(K21det01detseNA2) +
  geom_col(aes(x=occB,y=numNA/4000),color="black",fill="lightgrey") +
  geom_point(aes(x=occB,y=mean)) +
  geom_errorbar(aes(x=occB,ymax=mean+sd,ymin=mean-sd)) +
  scale_y_continuous(sec.axis = sec_axis(~.*4000, name = "Number of NA values")) +
  ylab("Mean SE of Estimate +/- SD, n=1000 simulations") +
  theme_classic()
p7i

grid1 <- plot_grid(p7a,p7b,p7c,p7d,p7e,p7f,p7g,p7h,p7i,nrow=3,ncol=3,
                   labels=c("K=7, p=0.5","K=14, p=0.5","K=21, p=0.5",
                            "K=7, p=0.3","K=14, p=0.3","K=21, p=0.3",
                            "K=7, p=0.1","K=14, p=0.1","K=21, p=0.1"))
grid1

