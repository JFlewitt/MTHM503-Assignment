# MTHM502 Summative Assessment

### Packages ##################################################################

install.packages("tidyr")
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("ggpmisc")
install.packages('extraDistr')
install.packages("reshape2") 
install.packages("plotly") 

library(tidyr)
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(extraDistr)
library(reshape2) 
library(plotly)

### Q2 ########################################################################

#I will run a large number of simulations, and count the number of simulations
#in which B wins. I will divide by the number of simulations to get the
#probability B wins
sims=10000
counter=0
days=14

#The idea is to run a simulation for each day, and use the number of supporters
#from the previous day
#This can be done with a loop, where the Band M supporters change each 
#iteration/day

#We need the probability of success each day, NOT the expected number of 
#supporters

#To understand the multiple day loop, a formula for the probability after 1 day
#need to be understood

#there were quite a few failed attempts here, such as one that only found the 
#expected value of supporters after so many days

#Maybe by creating a list for the possible number of supporters at which the 
#probabilities cross, then using the probability of having that many supporters
#and the probability of M having fewer than that many, then extracting each 
#value from the list to find the total probability (using law of total prob)
probs=list()
for(k in 0:184){
  for(i in k:175){
    BB=choose(175,i)*(0.996**i)*(0.004**(175-i))
  }
  for(i in k:184){
    MB=choose(184,i)*(0.005**i)*(0.995**(184-i))
  }
  for(i in 0:k){
    MM=choose(184,i)*(0.995**i)*(0.005**(184-i))
  }
  for(i in 0:k){
    BM=choose(175,i)*(0.004**i)*(0.996**(175-i))
  }
  probs=append(probs,BB+MB+MM+BM)
}

#probs

#This isn't going well

### Q3 ########################################################################

#generating quantiles for standard normal distribution for 95% CI
#both ends of distribution are equal
qnorm(0.975)

### Q4 ########################################################################

n=30

#the sum(log(yi)) term is a constant value, which can be defined here to use
#to solve the polynomial
sum=log(0.573)+log(0.770)+log(0.652)+log(0.827)+log(0.821)+log(0.789)+
  log(.898)+log(.718)+log(.382)+log(.668)+log(.647)+log(.477)+
  log(.661)+log(.380)+log(.870)+log(.794)+log(.783)+log(.732)+
  log(.629)+log(.777)+log(.600)+log(.724)+log(.553)+log(.693)+
  log(.687)+log(.935)+log(.494)+log(.411)+log(.530)+log(.478)

#solve the polynomial
rt=polyroot(c(6*sum+5*n,5*sum+2*n,sum))
#take only the real outputs
rt_real=Re(rt)
rt_real

#extract each real root separately 
root1=rt_real[1]
root2=rt_real[2]

#It is only a maximum likelihood estimator if it is actually a maximum, so check
#create a formula for second derivative of the polynomial to test for min/max
root1*2*sum+5*sum+2*n
root2*2*sum+5*sum+2*n
#This shows root1 is the maximum, so is the estimator used going forward

#using the maximum likelihood estimator (root1), define the pdf function to plot
pdffun=function(y){
  (root1+2)*(root1+3)*(1-y)*y**(root1+1)
}

#use ggplot to show the pdf using estimator root1
pdfplot=ggplot(data.frame(y=c(0,1)),aes(y=y))+
  stat_function(fun=pdffun)+
  labs(x='y',y='f(y;theta_hat)')+
  ggtitle('Probability density function using maximum likelihood estimator for theta')

### Q5 ########################################################################

#read the data
ozone <- read_csv("C:/Users/jonah/Downloads/ozone.csv")
#create a dataframe in rstudio for the data
ozonedf=data.frame(ozone)

#merge the variables into a single column and add a column describing the variable
#all of these have a corresponding ozone value
df=melt(ozonedf,id.vars='ozone',variable.name='Measured_Variables')

#remove any outliers

#form first and third quantiles
Q1=quantile(ozonedf$ozone,0.25)
Q3=quantile(ozonedf$ozone,0.75)
#define IQR
IQR=IQR(ozonedf$ozone)
#remove any datapoints that lie outside Q1-1.5*IQR to Q3+1.5*IQR
clean_df=subset(ozonedf,ozonedf$ozone>(Q1-1.5*IQR)&
                  ozonedf$ozone<(Q3+1.5*IQR))
number_outliers=abs(nrow(clean_df)-nrow(ozonedf))
#two outliers were removed
clean_df=melt(clean_df,id.vars='ozone',variable.name='Measured_Variables')

#Ultimately, I decided not to use the clean dataset, because I wasn't asked to
#and in marking, this might complicate things 

#plot a scatter diagram for all the data points together for visual assessment
#assign a separate colour to each variable
scatter=ggplot(df,aes(ozone,,x=value,y=ozone,col=Measured_Variables))+
  geom_point()+
  labs(x='Value (langleys), (farenheight), (mph)',y='Ozone Level (ppb)')
scatter

#add linear regression lines for each variable, regression formula, correlation
#coefficient 
all=ggplot(df,aes(ozone,,x=value,y=ozone,col=Measured_Variables))+
  geom_point()+
  labs(x='Value (langleys), (farenheight), (mph)',y='Ozone Level (ppb)')+
  stat_poly_line()+
  stat_poly_eq(aes(label=after_stat(eq.label)))+
  stat_cor(method = "pearson",label.x=200)
all

#plot each variable individually
rad=ggplot(data=ozone,aes(x=radiation,y=ozone))+
  geom_point(colour='red')+
  labs(x='Radiation (langleys)',y='Ozone Level (ppb)')+
  stat_poly_line()+
  stat_poly_eq(aes(label=after_stat(eq.label)))

temp=ggplot(data=ozone,aes(x=temperature,y=ozone))+
  geom_point(colour='green')+
  labs(x='Temperature (Farenheight)',y='Ozone Level (ppb)')+
  stat_poly_line()+
  stat_poly_eq(aes(label=after_stat(eq.label)))

win=ggplot(data=ozone,aes(x=wind,y=ozone))+
  geom_point(colour='blue')+
  labs(x='Wind (mph)',y='Ozone Level (ppb)')+
  stat_poly_line()+
  stat_poly_eq(aes(label=after_stat(eq.label)))

rad
temp
win

#repeat briefly for a quadratic regression line
all_quad=ggplot(df,aes(ozone,,x=value,y=ozone,col=Measured_Variables))+
  geom_point()+
  labs(x='Value (langleys), (farenheight), (mph)',y='Ozone Level (ppb)')+
  stat_poly_line(formula = y ~ poly(x, 2, raw = TRUE)) +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE),
               aes(label = after_stat(eq.label)))

rad_quad=ggplot(data=ozone,aes(x=radiation,y=ozone))+
  geom_point(colour='red')+
  labs(x='Radiation (langleys)',y='Ozone Level (ppb)')+
  stat_poly_line(formula = y ~ poly(x, 2, raw = TRUE)) +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE),
               aes(label = after_stat(eq.label)))

temp_quad=ggplot(data=ozone,aes(x=temperature,y=ozone))+
  geom_point(colour='green')+
  labs(x='Temperature (Farenheight)',y='Ozone Level (ppb)')+
  stat_poly_line(formula = y ~ poly(x, 2, raw = TRUE)) +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE),
               aes(label = after_stat(eq.label)))

win_quad=ggplot(data=ozone,aes(x=wind,y=ozone))+
  geom_point(colour='blue')+
  labs(x='Wind (mph)',y='Ozone Level (ppb)')+
  stat_poly_line(formula = y ~ poly(x, 2, raw = TRUE)) +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE),
               aes(label = after_stat(eq.label)))

all_quad
rad_quad
temp_quad
win_quad

#part b

#create the formula for multiple regression, using the linear method
mrmodel=lm(ozone~1+radiation+temperature+wind,data=ozonedf)
#show the coefficients of the variables in this method
summary(mrmodel)

#plot the fitted values and residuals
#add a line at y=0 to see the quality of the fit
rplot1=plot(fitted(mrmodel),resid(mrmodel),
     main="Fitted vs residual values for multiple regression model",
     xlab="Fitted Values",ylab="Residuals")
abline(0,0)
#create a boxplot of the residual values alongside this
rbox1=boxplot(resid(mrmodel),main="Residual boxplot",ylab="Residuals")

#part c

#create a new formula for the logarithmic model proposed
logformula=1+log(ozone)~log(radiation)+log(temperature)+log(wind)
logmodel=lm(logformula,data=ozonedf)
summary(logmodel)

#plot residuals and boxplot again
rplot2=plot(fitted(logmodel),resid(logmodel),
     main="Fitted vs residual values for the logarithmic model",
     xlab="Fitted Values",ylab="Residuals")
abline(0,0)

rbox2=boxplot(resid(logmodel),main="Residual boxplot",ylab="Residuals")

#create a q-q plot for the residuals, and a 45degree line for comparison
qplot=qqnorm(resid(logmodel))
qqline(resid(logmodel))

### PDFs ######################################################################
?pdf

pdf(file = "/Users/jonah/Documents/Assessement_plots.pdf")
print(pdfplot)
print(scatter)
print(all)
print(rad)
print(temp)
print(win)
print(all_quad)
print(rad_quad)
print(temp_quad)
print(win_quad)
dev.off()
