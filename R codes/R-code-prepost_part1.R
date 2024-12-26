################################################################################
## Title:       Pre-Post analysis with 2 time points - Part 1
## Programmer:  Mark Bounthavong
## Date:        11 December 2024
## Updated:     25 December 2024
## Updated by:  Mark Bounthavong
################################################################################

## Clear environment
rm(list = ls())


###############################
## Libraries
###############################
if (!require("pacman")) install.packages("pacman"); library("pacman")
p_load("ggplot2", 
       "psych", 
       "tidyverse",
       "lessR",
       "Hmisc",
       "readr",
       "lme4",
       "ggeffects",
       "margins",
       "gtsummary"
       )


## Load data from GitHub
urlfile <- "https://raw.githubusercontent.com/mbounthavong/R-tutorials/refs/heads/main/Data/long1.csv"

long1 <- read.csv(urlfile, header = TRUE)

long1$group <- as.factor(long1$group)
long1$time <- as.factor(long1$time)

## View data in long format
summary(long1)


#### Group & Summarize ####
long1 %>%
  group_by(group, time) %>%
  summarise(n_distinct(patientid),
            patientid = mean(endpoint1),
            sd(endpoint1))

## Summary stats
### Simple t test but does not take into account correlations
t.test(long1$endpoint1 ~ long1$group)


###################
## VISUALIZE DATA
##################
## Connected plot
plot1 <- long1 %>%
            group_by(group, time) %>%
            summarise(n_distinct(patientid),
                      patientid = mean(endpoint1),
                      sd(endpoint1))

ggplot(plot1, aes(x = time, y = patientid, col = group, group = group)) + 
    geom_point(aes(colour = factor(group))) + 
    geom_line(show.legend = FALSE) + 
    ylab("Average number of chocolates consumed") + 
    xlab("Before and After") 
    



###############################
## RESHAPE from LONG to WIDE
###############################

### Reshape the dataframe from long to wide
wide1 <- reshape(long1, idvar = "patientid", timevar = "time", direction = "wide")

### Rename the dataframe columns
wide1 <- wide1 %>%
            dplyr::rename(
                   group1 = group.1,
                   group2 = group.2, 
                   endpoint_pre = endpoint1.1, 
                   endpoint_post = endpoint1.2,
                   age = age.1
                   )

wide2 <- wide1 %>%
            dplyr::select(patientid,
                          group1,
                          endpoint_pre,
                          endpoint_post,
                          age)

## Calculate the difference between pre and post periods
wide2$diff <- wide2$endpoint_post - wide2$endpoint_pre

## We will create a new grouping variable for simplicity
wide2$group[wide2$group1 == 0] = 0
wide1$group[wide1$group1 == 1] = 1
table(wide2$group)


## Average difference between the groups
wide2 %>%
  group_by(group) %>%
  summarise(n_distinct(patientid),
            mean(diff),
            sd(diff))


t.test(wide2$diff ~ wide2$group, var.equal = FALSE)
describeBy(diff ~ group, data = wide2)


## Calculating the difference-in-differences bc t.test doesn't do this for us. 
exposed <- (wide2$diff)[wide2$group == 1]
unexposed <- (wide2$diff)[wide2$group == 0]

mean_diff.1 <- mean(exposed)
sd.1 <- sd(exposed)
var.1 <- var(exposed)
n.1 <- length(exposed)

mean_diff.0 <- mean(unexposed)
sd.0 <- sd(unexposed)
var.0 <- var(unexposed)
n.0 <- length(unexposed)


n <- n.1 + n.0
mean.diff <- mean_diff.1 - mean_diff.0
sd.diff <- sqrt(((n.0 - 1)*sd.0^2 + (n.1 - 1)*sd.1^2) / (n.0 + n.1 - 2))
se <- sqrt((var.0 / n.0) + (var.1 / n.1))


n
mean.diff # Average difference of the changes between the groups
sd.diff
se
mean.diff + 1.96 * se  ## UL of 95% CI
mean.diff - 1.96 * se  ## LL of 95% CI



###############################
## Linear regression approach
###############################

#### Crude model
lm1 <- glm(endpoint1 ~ group + time + group:time, family = "gaussian", data = long1)
summary(lm1)

lm1 %>%
  tbl_regression(intercept = TRUE,
                 estimate_fun = ~ style_number(.x, digits = 2))

#### Adjust for age
lm2 <- glm(endpoint1 ~ group + time + group:time + age, family = "gaussian", data = long1)
summary(lm2)

lm2 %>%
  tbl_regression(intercept = TRUE,
                 estimate_fun = ~ style_number(.x, digits = 2))  


