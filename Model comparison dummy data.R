
library(tidyverse)

#Create data for modelling

#Define age range
age <- 15:73

#Set probabilities and relationship to age
p_behav1 <- (age - 15)/(73-15) # linear increase with age
p_behav2 <- rep(0.9, length(age)) # no relationship with age
p_behav3 <- exp(-age/(15)) # exponential decline with age

#Plot probability variation with age
data.prob <- data.frame(cbind(age, p_behav1, p_behav2, p_behav3))
data.prob %>% ggplot(aes(x = age, y = p_behav1)) + geom_smooth(method = "lm", formula = y ~ poly(x,2))
data.prob %>% ggplot(aes(x = age, y = p_behav2)) + geom_smooth(method = "lm", formula = y ~ poly(x,2))
data.prob %>% ggplot(aes(x = age, y = p_behav3)) + geom_smooth(method = "lm", formula = y ~ poly(x,2))


#initialise vectors
behav1 <- vector()
behav2 <- vector()
behav3 <- vector()
subj_age <- vector()

# number of samples per age
s <- 1000

#Create data
for (i in 1:length(age)) {
  b1 <- rbinom(s, 1, p_behav1[i])
  b2 <- rbinom(s, 1, p_behav2[i])
  b3 <- rbinom(s, 1, p_behav3[i])
  subj_age <- append(subj_age, rep(age[i], s))
  behav1 <- append(behav1, b1)
  behav2 <- append(behav2, b2)
  behav3 <- append(behav3, b3)
}

#Bind data into dataframe
data <- data.frame(cbind(subj_age, behav1, behav2, behav3))

#Plot to check data
data %>% group_by(subj_age) %>% summarise(mean = mean(behav1)) %>% ggplot(aes(x = age, y = mean)) + geom_point()
data %>% group_by(subj_age) %>% summarise(mean = mean(behav2)) %>% ggplot(aes(x = age, y = mean)) + geom_point()
data %>% group_by(subj_age) %>% summarise(mean = mean(behav3)) %>% ggplot(aes(x = age, y = mean)) + geom_point()


#Create dataframe for interaction term modelling
data.int <- pivot_longer(data, cols = c("behav1", "behav2", "behav3"), names_to = "behav", values_to = "prosociality")

#Fit models
m1 <- glm(prosociality ~ poly(subj_age, 2), family = "binomial", data = data.int)
summary(m1)
m2 <- glm(prosociality ~ behav*poly(subj_age, 2), family = "binomial", data = data.int)
summary(m2)

#Akaike weight method
mA1 <- lm(subj_age ~ behav1, data = data)
summary(mA1)
mA2 <- lm(subj_age ~ behav2, data = data)
summary(mA2)
mA3 <- lm(subj_age ~ behav3, data = data)
summary(mA3)

AICcmodavg::aictab(cand.set  = list(mA1, mA2, mA3), modnames = c("Linear +ve", "None", "Exp -ve"))
AICcmodavg::aictab(cand.set  = list(mA2, mA3), modnames = c("None", "Exp -ve"))

