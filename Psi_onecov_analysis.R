# Packages ####
library(dplyr)
library(ggplot2)
library(ggridges)
library(viridis)

# Load data ####
data <- get(load("Psi_onecov_all.Rdata"))
rm(resultsall)

data$K <- as.factor(data$K)
levels(data$K) <- c("K = 7","K = 14","K = 21")

data$true.pdet.f <- as.factor(data$true.pdet)
levels(data$true.pdet.f) <- c("Pdet = 0.1", "Pdet = 0.3", "Pdet = 0.5")

# Visualise model failures
data.na <- data %>% filter(is.na(X.pval)) 

summary(data.na$K)

p1 <- ggplot(data.na, aes(x=B1)) +
  geom_histogram() +
  facet_grid(K ~ true.pdet.f) +
  ylab("Number of model failures") +
  theme_gray()
p1

# Psi Accuracy ####
data$bias <- data$X.est - data$B1
data$absbias <- abs(data$bias)

data.trimmed <- data %>% filter(between(bias,-5,5))
data.extreme <- data %>% filter(bias < -5 | bias > 5)

p2a <- ggplot(data.trimmed, aes(x=B1, y=bias)) +
  geom_bin2d() +
  scale_fill_continuous(type = "viridis") +
  ylab("Bias") +
  labs(title="Excluding Bias > 5 and < -5") +
  facet_grid(K ~ true.pdet.f)
p2a

p2b <- ggplot(data.extreme, aes(x=B1, y=bias)) +
  geom_bin2d() +
  scale_fill_continuous(type = "viridis") +
  ylab("Bias") +
  labs(title="Bias > 5 and < -5 Only") +
  facet_grid(K ~ true.pdet.f)
p2b

tapply(data.trimmed$absbias, data.trimmed$K, median)
tapply(data$absbias, data$K, median)


# Psi Precision ####
p3a <- ggplot(data, aes(x=B1, y=X.se, group=K)) +
  geom_bin2d() +
  theme_bw() +
  scale_fill_continuous(type = "viridis") +
  ylab("Standard Error of B1 Estimate") +
  facet_grid(K ~ true.pdet.f)
p3a

data.trimmed2 <- data %>% filter(X.se < 5)

p3b <- ggplot(data.trimmed2, aes(x=B1, y=X.se, group=K)) +
  geom_bin2d() +
  theme_bw() +
  scale_fill_continuous(type = "viridis") +
  ylab("Standard Error of B1 Estimate") +
  labs(title = "Standard errors < 5 Only") +
  facet_grid(K ~ true.pdet.f)
p3b

data.trimmed3 <- data %>% filter(X.se < 1.5)

p3c <- ggplot(data.trimmed3, aes(x=B1, y=X.se, group=K)) +
  geom_bin2d() +
  theme_bw() +
  scale_fill_continuous(type = "viridis") +
  ylab("Standard Error of B1 Estimate") +
  labs(title = "Standard errors < 1.5 Only") +
  facet_grid(K ~ true.pdet.f)
p3c


# Pdet Accuracy ####
data$pdet.bias <- data$det.intercept.est - data$true.pdet.logit

p4a <- ggplot(data, aes(x = pdet.bias, group=K, fill=K)) +
  geom_density(adjust=1.5, alpha=0.4) + 
  facet_grid(true.pdet.f ~ .) +
  scale_fill_viridis(discrete=TRUE, option="viridis") +
  theme_classic() +
  xlab("Bias (logit scale)")
p4a


p4b <- ggplot(data, aes(x = det.intercept.est, group=K, fill=K)) +
  geom_density(adjust=1.5, alpha=0.4) + 
  facet_grid(true.pdet.f ~ .) +
  scale_fill_viridis(discrete=TRUE, option="viridis") +
  theme_classic() +
  xlab("Estimated Detection Probability")
p4b             
             
# Pdet Precision ####
p5a <- ggplot(data, aes(x = det.intercept.se, group=K, fill=K)) +
  geom_density(adjust=1.5, alpha=0.4) + 
  facet_grid(true.pdet.f ~ .) +
  scale_fill_viridis(discrete=TRUE, option="viridis") +
  theme_classic() +
  xlab("SE of Estimated Detection Probability")
p5a


