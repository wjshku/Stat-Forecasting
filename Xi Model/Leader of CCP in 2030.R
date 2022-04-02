library(tidyverse)
#Testing the model
{
  Leader_of_CCP_in_2030 %>%
    filter(Country == "China")
  
  China <- filter(Leader_of_CCP_in_2030, Country == "China")[-6,]
  cm <- mean(China$Inpower)
  csd <- sd(China$Inpower)
  am <- mean((China$Death-China$Born)[1:4])
  asd <- sd((China$Death-China$Born)[1:4])
  
  #inpower ~ N(cm, csd), find prob(inpower>2030-2013)
  t_prez <- 2022-2012
  t_needed <- 2030-2012
  
  #adjust his mean, considering already secured his third term
  xim <- cm+5
  xisd <- csd
  p_abs <- 1-pnorm(t_needed,xim,xisd)
  p_cond <- (1-pnorm(t_needed,xim,xisd))/(1-pnorm(t_prez,xim,xisd))
  
  #age ~ N(am, asd), find prob(age>2030-1953)
  l_prez <- 2022-1953
  l_needed <- 2030-1953
  
  #conditioning on his age now
  a_abs <- 1-pnorm(l_needed,am,asd)
  a_cond <- (1-pnorm(l_needed,am,asd))/(1-pnorm(l_prez,am,asd))
  
  #If we consider the ambition and power of Xi right now, it makes sense to narrow down our reference class to those real paramounts
  China <- filter(Leader_of_CCP_in_2030, Country == "China")[c(-2,-5,-6),]
  
  #Model for his reign, assuming he will be longevous
  xi_inpower_p <- function(leaders, aim_year, adj, t_now, startdate){
    cm <- mean(leaders$Inpower)
    csd <- sd(leaders$Inpower)
    
    #inpower ~ N(cm, csd), find prob(inpower>2030-2013)
    t_prez <- t_now-startdate
    t_needed <- aim_year-startdate
    
    #adjust his mean, considering already secured his third term
    xim <- cm+adj
    xisd <- csd
    p_abs <- 1-pnorm(t_needed,xim,xisd)
    p_cond <- (1-pnorm(t_needed,xim,xisd))/(1-pnorm(t_prez,xim,xisd))
    
    return(p_cond)
  }
  
  #Model for his life expectancy
  xi_inpower_a <- function(leaders, aim_year, t_now, birth){
    am <- mean((leaders$Death-leaders$Born))
    asd <- sd((leaders$Death-leaders$Born))
    
    #age ~ N(am, asd), find prob(age>2030-1953)
    l_prez <- t_now-birth
    l_needed <- aim_year-birth
    
    #conditioning on his age now
    a_abs <- 1-pnorm(l_needed,am,asd)
    a_cond <- (1-pnorm(l_needed,am,asd))/(1-pnorm(l_prez,am,asd))
    
    return(a_cond)
  }
  
  #Outputs the integrated result
  xi_inpower <- function(leaders, aim_year, adj, t_now, startdate, birth){
    p_cond <- xi_inpower_p(leaders, aim_year, adj, t_now, startdate)
    a_cond <- xi_inpower_a(leaders,aim_year, t_now, birth)
    return(p_cond*a_cond)
  }
  
  #Final Result, 0.56 and 0.72 respectively
  xi_inpower(China, 2030)
  xi_inpower_p(Leader_of_CCP_in_2030, 2030, 10)
  xi_inpower_p(China, 2030, 10)
}


#Event-based Adjustment: This is an attempt to estimate the distinct mean, by saying that
#it influences the outcome of some binomial events. N ~ Bino(p), p ~ Beta(alpha + mean, beta)
#The larger the mean is, the larger p will be
{
  adj <- 1
  qbeta(0.5,adj+1,1)
  qbeta(0.95,adj+1,1)
  
  #Find MLE based one a bino+beta model
  likelihood <- function(alpha,beta,n,x){
    return(beta(alpha+x,beta-x+n)/beta(alpha,beta))
  }
  
  esti_adj <- function(n,x,alpha,beta,min,max){
    best <- min
    like <- 0
    for(i in seq(min,max,length = 100)){
      if(like < likelihood(i + alpha,beta,n,x)){
        best <- i
        like <- likelihood(i + alpha,beta,n,x)
      }
    }
    return(best)
  }
  
  n <- 1
  x <- 1
  alpha <- 1.5
  beta <- 1.5
  min <- 1
  max <- 10
  adj <- esti_adj(n,x,alpha,beta,min,max)
}


#Run the model here

#Step1: Input data
leaders <- China
startdate <- 2012
birth <- 1953
t_now <- 2022
aim_year <- 2030
{t_prez <- t_now-startdate
  t_needed <- aim_year-startdate
  l_prez <- t_now-birth
  l_needed <- aim_year-birth}

#Step2: If you want to make adjustment based some events. Do the adjustment here, for simplicity, let's set alpha = 1. min and max is the bound for your adjustment
#n is the total number of events. x is the successful ones.
n <- 1
x <- 1
min <- 1
max <- 5
{alpha <- 1}
#Set your beta here. Pretending you know nothing about any details, 
#this event happens with probability alpha/(alpha + beta).
#Choose beta such that this number fits your intuition.
p <- 0.5
{beta <- alpha/p - alpha}
#Then the adjustment is here
adj <- esti_adj(n,x,alpha,beta,min,max)

#Step3: Final result here
xi_inpower(leaders, 2030, adj, t_now, startdate, birth)


