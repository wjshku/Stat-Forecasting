library(tidyverse)

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
#And get a new set of cm,csd,am,asd
xi_inpower_p <- function(leaders, aim_year, adj){
  cm <- mean(leaders$Inpower)
  csd <- sd(leaders$Inpower)

  #inpower ~ N(cm, csd), find prob(inpower>2030-2013)
  t_prez <- 2022-2012
  t_needed <- aim_year-2012
  
  #adjust his mean, considering already secured his third term
  xim <- cm+adj
  xisd <- csd
  p_abs <- 1-pnorm(t_needed,xim,xisd)
  p_cond <- (1-pnorm(t_needed,xim,xisd))/(1-pnorm(t_prez,xim,xisd))
  
  return(p_cond)
}

xi_inpower_a <- function(leaders, aim_year){
  am <- mean((leaders$Death-leaders$Born))
  asd <- sd((leaders$Death-leaders$Born))
  
  #age ~ N(am, asd), find prob(age>2030-1953)
  l_prez <- 2022-1953
  l_needed <- aim_year-1953
  
  #conditioning on his age now
  a_abs <- 1-pnorm(l_needed,am,asd)
  a_cond <- (1-pnorm(l_needed,am,asd))/(1-pnorm(l_prez,am,asd))
  
  return(a_cond*p_cond)
}

#Final Result, 0.56 and 0.72 respectively
xi_inpower(China, 2030)
xi_inpower_p(Leader_of_CCP_in_2030, 2030, 10)
xi_inpower_p(China, 2030, 10)

#Event-based Adjustment
adj <- 1
qbeta(0.5,adj+1,1)
qbeta(0.95,adj+1,1)

#Find MLE based one a bino+beta model
likelihood <- function(alpha,beta,n,x){
  return(beta(alpha+x,beta-x+n)/beta(alpha,beta))
}

esti_adj <- function(n,x,beta,min,max){
  best <- min
  like <- 0
  for(i in seq(min,max,length = 100)){
    if(like < likelihood(i + beta,beta,n,x)){
      best <- i
      like <- likelihood(i + beta,beta,n,x)
    }
  }
  return(best)
}

#Test the model based on our assumptions of min, max
adj <- esti_adj(1,1,1.5,1,10)
xi_inpower_p(Leader_of_CCP_in_2030, 2030, adj)


