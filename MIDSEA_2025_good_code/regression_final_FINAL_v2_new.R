setwd("C:/Users/MyUser/Downloads/summer_school_analysis/student_performance")

x <- read.csv("C:/Users/MyUser/Downloads/summer_school_analysis/student_performance/data.csv")
head(x)
summary(x$math_score)
x$math_score[20]<-51

hist(x$math_score)
temp = density(x$math_score)
plot(temp$x, temp$y, type = "l", main="Math score distribution")
temp <- mean(x$math_score)
xx <- sd(x$math_score)
x = seq(0,100,by=0.1)
y = dnorm(x, temp, xx)
lines(x,y, col="red")

plot(x$study_hours, x$math_score, main="Math Scores", col="red", pch=16, cex=1.2)
abline(lm(math_score ~ study_hours, data=x), col="red", lwd=2)
cor(x$study_hours, x$math_score)

# effect analysis
library(dplyr)
data<- x
mean<-mean(data$math_score)
tmp<-data$math_score > mean + 0.5*15.8
high<-x[tmp,]
n_high<-nrow(high)
total<-nrow(data)
percentage<-n_high/total*100
x$math_score[x$math_score < 40]<-40

model <-lm(math_score ~ study_hours + grade_level, data=x)
summary(model)
plot(x$study_hours, x$math_score, main="Performance vs Study Hours", xlab="Hours", ylab="Score", col="darkblue", pch=19)
abline(model, col="red", lwd=3)
points(high$study_hours, high$math_score, col="red", pch=19, cex=1.5)



# may be different by school??
source("helper_functions.R")
x$school_name <- convert_abbrev(x$school)
unique(x$school_name)

par(mfrow = c(1,3))
data <- x %>% filter(school_name == "Northside High School")
mean <- mean(data$math_score)
tmp <- data$math_score > mean + 0.5*15.8
high <- x[tmp,]
n_high <- nrow(high)
total <- nrow(data)
percentage <- n_high/total*100
x$math_score[x$math_score < 40] <- 40
model <- lm(math_score ~ study_hours + grade_level, data=x)
summary(model)
plot(x$study_hours, x$math_score, main="Northside High School", xlab="Hours", ylab="Score", col="darkblue", pch=19)
abline(model, col="red", lwd=3)
points(high$study_hours, high$math_score, col="red", pch=19, cex=1.5)

data <- x %>% filter(school_name == "Central High School")
mean <- mean(data$math_score)
tmp <- data$math_score > mean + 0.5*15.8
high <- x[tmp,]
n_high <- nrow(high)
total <- nrow(data)
percentage <- n_high/total*100
x$math_score[x$math_score < 40] <- 40
model <- lm(math_score ~ study_hours + grade_level, data=x)
summary(model)
plot(x$study_hours, x$math_score, main="Central High School", xlab="Hours", ylab="Score", col="darkblue", pch=19)
abline(model, col="red", lwd=3)
points(high$study_hours, high$math_score, col="red", pch=19, cex=1.5)

data <- x %>% filter(school_name == "Westfield High")
mean <- mean(data$math_score)
tmp <- data$math_score > mean + 0.5*15.8
high <- x[tmp,]
n_high <- nrow(high)
total <- nrow(data)
percentage <- n_high/total*100
x$math_score[x$math_score < 40] <- 40
model <- lm(math_score ~ study_hours + grade_level, data=x)
summary(model)
plot(x$study_hours, x$math_score, main="Westfield High", xlab="Hours", ylab="Score", col="darkblue", pch=19)
abline(model, col="red", lwd=3)
points(high$study_hours, high$math_score, col="red", pch=19, cex=1.5)
