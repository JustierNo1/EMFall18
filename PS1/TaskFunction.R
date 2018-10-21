#Author: Patrick Glettig
#Date: 05.10.2018
#Title: Problemset 1 Empirical Methods, Function for solving questions 2 a)-d)

solveTask <- function(N=1,R=200){
  library(ggplot2)#for the histogram
  #Create empty DF first to be filled
  samples <- data.frame(matrix(data=NA,nrow=R,ncol=N))
  
  for (i in 1:R){#we need 200 replications
    samples[i,] <- rexp(n=N) #draw 5 observations of the exponential distribution
  }
  #Rename column names
  names(samples) <- paste0('x_',1:N)
  
  samples['x_bar_avg']<-apply(samples,MARGIN = 1,FUN = mean)#calculate average per sample
  plot <- ggplot(samples, aes(x=x_bar_avg)) + geom_histogram(binwidth=0.1)
  x_bar <- mean(samples$x_bar_avg)
  s_x_bar <- var(samples$x_bar_avg)
  result <- list(plot,x_bar,s_x_bar)
  names(result) <- c('Histogram','x_bar','s_x_bar')
  return(result)
}

library(extrafont)
loadfonts()

solveTask <- function(N=1,R=200){#define the default parameters as for the first task
  library(ggplot2)#for the histogram
  #Create empty DF first to be filled
  samples <- data.frame(matrix(data=NA,nrow=R,ncol=N))
  
  for (i in 1:R){#R is number of replications
    samples[i,] <- rexp(n=N) #N is observations of the exponential distribution
  }
  #Rename column names
  names(samples) <- paste0('x_',1:N)
  
  samples['x_bar_avg']<-apply(samples,MARGIN = 1,FUN = mean)#calculate average per sample
  
  #Now we do the plot
  plot <- ggplot(samples, aes(x=x_bar_avg)) +     geom_histogram(binwidth=0.1)+
    theme(text = element_text(size=10, family="LM Roman 10"))+
    ggtitle(paste0('Histogram with N= ',N,' and R=',R))
  x_bar <- mean(samples$x_bar_avg)
  s_x_bar <- var(samples$x_bar_avg)
  result <- list(plot,x_bar,s_x_bar)
  names(result) <- c('Histogram','x_bar','s_x_bar')
  return(result)
}

solveTask(N=5,R=200)
