#A function that outputs the probability of Poi large than n
poi <- function(lambda, n){
  p <- 1
  if(n < 0){
    return(p)
  }
  for(i in 0:n){
    p <- p - exp(-lambda)*lambda^(i)/factorial(i)
  }
  return(p)
}

#starting time is 1991
Nbytime <- c(1,2,3,4,5,6,7,8,9)

#Total Number of breakthroughs recognized at the end of year tp
NumofBreak <- function(breakthroughs,tp){
  for(i in length(breakthroughs):1){
    if(breakthroughs[i] <= tp){
      return(i)
    }
  }
}

prob_before <- function(tp, lambda_hat, np, n_break){
  prob <- poi((2021-tp)*lambda_hat,n_break-np-1)
  return(prob)
}

lambda_esti <- function(alpha, beta, tp, np){
  period <- tp - 1991 + 1
  return((np+alpha)/(beta+period))
}

#Run the program here
Breakthroughs1 <- c(1991,2001,2003,2013,2017,2018,2019)
Breakthroughs <- c(1991,2001,2003,2008,2013,2017,2018,2019,2019,2020,2020)

alpha <- 1
beta <- 0.5

n_breaks <- 9:12
t <- 2016:2020
trials <- data.frame(matrix(rep(1,length(n_breaks)*length(t)),ncol = length(t)), row.names = n_breaks)
colnames(trials) <- t
for(i in 1:length(n_breaks)){
  for(j in 1:length(t)){
    np <- NumofBreak(Breakthroughs,t[j])
    lambda_hat <- lambda_esti(alpha, beta, t[j], np)
    #print(lambda_hat)
    trials[i,j] <- prob_before(t[j],lambda_hat, np, n_breaks[i])
  }
}

round(trials,2)

(length(Breakthroughs1) + alpha)/(beta + 2020 - 1991)
