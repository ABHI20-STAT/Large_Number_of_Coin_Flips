# R code for generating and visualizing a large number of coin flips. 
# This was created with the intention of demonstrating short-term vs. long-term probability of an outcome. 
# It is assumed that each trial is independent. Animated plots are provided to visualize the change in probability 
# in a large number of trials.
# ******************************************************************************************************************
# Required packages to incorporate the problem: 

install.packages(c("ggplot2", "dplyr", "gganimate", "gifski", "png", "installr"))
library(ggplot2)
library(dplyr)
library(gganimate)
library(gifski)
library(png)
library(installr)

# ******************************************************************************************************************
# Initial Fair Coin Simulation and Data Frame Preparation:
# The first block of code accomplished the following:
# 1.Load appropriate R packages.
# 2.Simulate 5,000 coin flips from a fair coin.
# 3.Create a data frame that contains trial number and outcome of an individual coin flip.
# ******************************************************************************************************************
flips <- sample(c(0, 1), 5000, replace = TRUE)
flips <- matrix(flips, ncol = 1)
flips <- as.data.frame(flips)
Trial <- seq(1, 5000, 1)
Trial <- as.data.frame(Trial)
flipsim <- cbind(flips, Trial)
colnames(flipsim) <- c("Heads", "Trial")
#*******************************************************************************************************************
# Calculating Cumulative Percentage of Heads:
# This block of code carries out a calculation of cumulative percentage of heads at the conclusion of the 
# recorded trial.
#******************************************************************************************************************
flipsim[,"Cum_Heads"] <- cumsum(flipsim$Heads)
flipsim <- flipsim %>% mutate(Pct_Heads = Cum_Heads/Trial)
head(flipsim)
#*******************************************************************************************************************
# Second Simulation (Weighted Coin) and Preparation of Data Frame:
# The prior steps are undertaken for a second simulation condition. 
# This condition will represent a situation involving a weighted coin (58% chance of landing heads face up).
#*******************************************************************************************************************
fakeflips <- sample(c(0, 1), 5000, replace = TRUE, prob = c(.42, .58))
flipsim2 <- flipsim
flipsim2$Heads <- fakeflips
flipsim2[, "Cum_Heads"] <- cumsum(flipsim2$Heads)
flipsim2 <- flipsim2 %>% mutate(Pct_Heads = Cum_Heads/Trial)
head(flipsim2)
#*******************************************************************************************************************
# Animated Line Plots of Cumulative Percentage:
# Line plots are produced that show how percentage of heads outcomes changes as the number of trials increases
# ******************************************************************************************************************
fair_plot <- flipsim %>% ggplot(aes(y = Pct_Heads, x = Trial)) + ggtitle("Percentage of Heads \n Fair Coin") + geom_line() + geom_segment(aes(xend = 5000, yend = Pct_Heads), linetype = 2,color = "red") + geom_point(size = 2) + transition_reveal(Trial) + ylim(0,1) + coord_cartesian(clip = "off") + theme(plot.title = element_text(hjust = 0.5)) 
fair_plot
# ******************************************************************************************************************
# Percentage of Heads weighted coin; P(H) = 0.58
fake_plot <- flipsim2 %>% ggplot(aes(y = Pct_Heads, x = Trial)) + ggtitle("Percentage of Heads \n Weighted Coin; P(H) = .58") + geom_line() + geom_segment(aes(xend = 5000, yend = Pct_Heads), linetype = 2, color = "red") + geom_point(size = 2) + transition_reveal(Trial) + ylim(0,1) + coord_cartesian(clip = "off") + theme(plot.title = element_text(hjust = 0.5)) 
fake_plot
#*******************************************************************************************************************
# Calculate Upper and Lower Bounds of Estimated Percentage Based on Standard Error
# In order to create ribboned error bands around the line plots, standard errors and lower/upper boundaries
# need calculated at the conclusion of each trial for both simulation data frames.
#*******************************************************************************************************************
flipsim <- flipsim %>% rowwise() %>% mutate(lower = max(0, Pct_Heads - 2*(sqrt((Pct_Heads*(1-Pct_Heads))/Trial))))
flipsim <- flipsim %>% rowwise() %>% mutate(upper = min(1, Pct_Heads + 2*(sqrt((Pct_Heads*(1-Pct_Heads))/Trial))))
flipsim2 <- flipsim2 %>% rowwise() %>% mutate(lower = max(0, Pct_Heads - 2*(sqrt((Pct_Heads*(1-Pct_Heads))/Trial))))
flipsim2 <- flipsim2 %>% rowwise() %>% mutate(upper = min(1, Pct_Heads + 2*(sqrt((Pct_Heads*(1-Pct_Heads))/Trial))))
#*******************************************************************************************************************
# Animated Line Plots of Cumulative Percentage (with Error Bands)
# The same line plots as before are created, but now include the calculated error bands.
#*******************************************************************************************************************
# Percentage of Heads(Fair Coin)
fair_plot2 <- flipsim %>% ggplot(aes(y=Pct_Heads,x=Trial)) + ggtitle("Percentage of Heads \n Fair Coin") + geom_line() + geom_segment(aes(xend = 5000, yend=Pct_Heads), linetype=2,color="red") + geom_point(size=2) + geom_ribbon(aes(ymin = lower, ymax = upper), fill = "red", alpha = .5) + transition_reveal(Trial) + coord_cartesian(clip="off") + ylim(0,1) + theme(plot.title = element_text(hjust = 0.5))
fair_plot2 
# Percentage of Heads(Weighted Coin) with P(H) = 0.58
fake_plot2 <- flipsim2 %>% ggplot(aes(y=Pct_Heads,x=Trial)) + ggtitle("Percentage of Heads \n Weighted Coin; P(H) = .58") + geom_line() + geom_segment(aes(xend = 5000, yend=Pct_Heads), linetype=2,color="red") + geom_point(size=2) + geom_ribbon(aes(ymin = lower, ymax = upper), fill = "red", alpha = .5) + transition_reveal(Trial) + coord_cartesian(clip="off") + ylim(0,1) + theme(plot.title = element_text(hjust = 0.5))
fake_plot2
