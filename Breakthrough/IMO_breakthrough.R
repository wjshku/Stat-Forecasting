#Code and Testing
{
  #A function that outputs the probability of Poi large than n(excluding equal)
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
  
  #Total Number of breakthroughs recognized at the end of year tp
  NumofBreak <- function(breakthroughs,tp){
    for(i in length(breakthroughs):1){
      if(breakthroughs[i] <= tp){
        return(i)
      }
    }
  }
  
  #Probability of more than n_break-np-1 events happening during tp~t
  prob_before <- function(t, tp, lambda_hat, np, n_break){
    prob <- poi((t-tp)*lambda_hat,n_break-np-1)
    return(prob)
  }
  
  #Estimate lambda based on Bayesian approach
  lambda_esti <- function(startdate, alpha, beta, tp, np){
    period <- tp - startdate + 1
    return((np+alpha)/(beta+period))
  }
  
  #Final model
  #List of parameters: 
  #Breakthroughs is the list of recognized breakthroughs
  #n_breaks is the preset number of breakthroughs need for something to happen
  #t is the time points of interest
  #tp is the present time
  #alpha and beta are the hyperparameters
  #startdate is the assumed starting date of all breakthroughs
  #np is number at the present time
  
  breakthrough <- function(Breakthroughs, n_breaks, t, tp, alpha, beta, startdate){
    trials <- data.frame(matrix(rep(1,length(n_breaks)*length(t)),ncol = length(t)), row.names = n_breaks)
    colnames(trials) <- t
    np <- NumofBreak(Breakthroughs,tp)
    lambda_hat <- lambda_esti(startdate, alpha, beta, tp, np)
    for(i in 1:length(n_breaks)){
      for(j in 1:length(t)){
        trials[i,j] <- prob_before(t[j], tp,lambda_hat, np, n_breaks[i])
      }
    }
    trials <- round(trials,2)
    return(trials)
  }
  
  #input is the dataframe, prob is the vector of percentiles, col is the index of probability column in input
  findpercent <- function(input,prob,col){
    ind <- prob
    i <- 1
    for(j in seq(1,length(input[,col]))){
      if(input[j,col] < prob[i] && input[j+1,col] >= prob[i]){
        ind[i] <- j
        i <- i + 1
        if(i > length(prob)){
          break
        }
      }
    }
    return(input[ind,])
  }
  
  #Run the program here
  alpha <- 1
  beta <- 5
  startdate <- 1960
  tp <- 2022
  
  #This can be used to adjust beta
  (length(Breakthroughs) + alpha)/(beta + 2022 - startdate)
  Breakthroughs <- c(1970,1980,1989,1997,2010,2016,2018,2022)
  
  n_breaks <- 10:12
  t <- seq(2025,2080)
  
  output <- breakthrough(Breakthroughs, n_breaks, t, tp, alpha, beta, startdate)
  aggre <- data.frame(year = as.integer(colnames(output)),prob = colMeans(output), row.names = NULL)
  findpercent(aggre,c(0.25,0.5,0.75),2)
  
  #Some scenarios
  beta <- 10
  Breakthroughs <- c(1970,1980,1989,1997,2010,2016,2018,2022)
  n_breaks <- 10
  
  beta <- 10
  Breakthroughs <- c(1970,1980,1989,1997,2010,2016,2018,2022)
  n_breaks <- 11
  
  beta <- 10
  Breakthroughs <- c(1970,1980,1989,1997,2010,2016,2018,2022)
  n_breaks <- 12
}

#How to use this model
#Step1: Choose beta(Note: 1/beta is the expected breakthroughs in one year needed for )
beta <- 5
n_breaks <- 10

#Step2: Choose breakthroughs(Note: this should be those in your "10 milestones" list)
Breakthroughs <- c(1970,1980,1989,1997,2010,2016,2018,2022)

#Step3: Adjust beta(Note: Elambda is the numbers of breakthroughs your model predicts in next year,
#See if it fits your intuition)
Elambda <- (length(Breakthroughs) + alpha)/(beta + 2022 - startdate)

#Step4: Run the model
t <- seq(2025,2080)

output <- breakthrough(Breakthroughs, n_breaks, t, tp, alpha, beta, startdate)
aggre <- data.frame(year = as.integer(colnames(output)),prob = colMeans(output), row.names = NULL)
findpercent(aggre,c(0.25,0.5,0.75),2)

  
