doZscores = function(x)
  
{
  #Initialize variables
  
  #sum all the values in the vector
  vec_sum <- sum(x)
  
  #number of elements in the vector
  vec_len <- length(x)
  
  #vector mean
  vec_mean <- mean(x)
  
  #vector standard deviation
  vec_std = sd(x)
  
  #initialize z_scores vector
  z_scores <- rep(0,vec_len)
  
  
  #Zscores calculation
  for(i in seq(x))
  {
    z_scores[i] = ((x[i]) - vec_mean) /vec_std
  }
  
  #return Zscores  
  return (z_scores)
  
}


doSampleVariance = function(x, method)
{
  #Initialize variables
  vec_naive_squared = 0
  vec_2pass_sum = 0
  vec_variance = 0
  vec_squared = 0
  vec_temp = 0
  
  #sum all the values in the vector
  vec_sum <- sum(x)
  
  #number of elements in the vector
  vec_len <- length(x)
  
  
  #vector mean
  vec_mean <- mean(x)
  
  if(method=="naive")
  {
    
    for(i in seq(x)){
      vec_temp = (x[i])^2  
      vec_squared = (vec_squared  + vec_temp) 
    }
    #vec_variance = (SumSq ??? (Sum × Sum) / n) / (n ??? 1)
    vec_variance = (vec_squared - (vec_sum * vec_sum) / vec_len) / (vec_len -1)
  }
  else
  {
    # two-pass algorithm
    for(i in seq(x)){
      vec_temp = (x[i] - vec_mean)^2  
      vec_squared = (vec_squared  + vec_temp) 
    }
    
    
    vec_variance = vec_squared / (vec_len -1)
  }
  
  return (list(sum=vec_sum,sum_sqr=vec_squared,variance=vec_variance  ))
  
}


doMode = function(v)
{
  
  #find unique sets in the vector
  ux <- unique(v)
  tab <- tabulate(match(v, ux))
  results <- ux[tab == max(tab)]
  
  return(results)
}


doSummary = function(x)
{
  
  
  cat("Record lengeth=",(length(x)), "\n")
  cat("Number of NA's=",length(x[is.na(x)]), "\n")
  cat("Mean=",mean(x), "\n")
  cat("Median=",median(x,na.rm = FALSE), "\n")
  
  mode_result<- doMode(x)
  cat("Mode results=", mode_result, "\n")
  
  vec_results <- doSampleVariance(x,"naive")
  cat ("Naive sum =", vec_results$sum ,"\n",sep="\t")
  cat ("Naive sum squard =", vec_results$sum_sqr ,"\n",sep="\t")
  cat ("Naive variance =", vec_results$variance ,"\n",sep="\t")
  
  doSampleVariance(x,"")
  cat ("2-pass sum =", vec_results$sum ,"\n",sep="\t")
  cat ("2-pass sum squard =", vec_results$sum_sqr ,"\n",sep="\t")
  cat ("2-pass variance =", vec_results$variance ,"\n",sep="\t")
  
  
  cat("Built-in Standard deviation function=", sd(x), "\n")  
  
  #calculate Z-scores
  zScores <- doZscores(x)
  
  #plot original data vs. Z-scores
  plot(x,zScores, pxlab="Personality ?", ylab="Z score", main="Personality Zscores", ylim=c(-2,2), xlim=c(1,5), pch=15, col="blue")
  
  # fit a line to the points
  myline.fit <- lm(zScores ~ x)
  
  # get information about the fit
  #summary(myline.fit)
  
  # draw the fit line on the plot
  abline(myline.fit)
  
}






