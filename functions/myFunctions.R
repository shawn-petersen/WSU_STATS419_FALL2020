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




#**************************************************************************************
# IMBD functions
#**************************************************************************************
# Function, return inflation dollars from year
inflation_dollar_from_year = function(x, yrs)
{
  #return inflation dollar from a year
  return (x[which(x$year == yrs),2])
}


# function to convert a vector of raw dollars to inflation dollar 
# per year
convert_inflation = function(x, base_year,inflation_vector)
{
  
  #initialize inflation vector
  inflation_vec  <- rep(0,length(x))
  
  #year = 2020 to adjust inflation to
  inflation_basis <- inflation_dollar_from_year(inflation_vector, base_year)
  
  for(i in 1:nrow(x))
  {
    movie_year = x[i,"year"]
    money_earned_year = x[i,"millions"]
    
    #get $$$ 
    dollar_inf_year <- inflation_dollar_from_year(inflation_vector, movie_year)
    
    #adjust $$$
    #money_adjusted = (dollar_inf_year/inflation_basis) * money_earned_year
    money_adjusted = (inflation_basis/dollar_inf_year) * money_earned_year
    
    #put adjusted $$$ from inflation into vector
    inflation_vec[i] = money_adjusted
    
  }
  return(inflation_vec)
}



education = function(one)
{
  result = list();
  result$who 		= one;
  result$think 	= c("intensitively", "critically");
  result$goal 	= "intelligences + character";
  result;	
}


me = education("monte");

# n > 1 ... 

# Research question:  who is a better actor?  Will Smith?  Denzel Washington?

##
myMatrix = matrix ( c (
  1, 0, 2,
  0, 3, 0,
  4, 0, 5
), nrow=3, byrow=T);

transposeMatrix = function(mat)
{
  t(mat);	
}

#rotateMatrix90(mat)	
#rotateMatrix180(mat)	
#rotateMatrix270(mat)
# 3x3 matrix ... ## matrix multiplication

# install.packages("stringr", dependencies=T);
library(stringr);	
# install.packages("rvest", dependencies=T);
library(rvest);	

# Research question:  who is a better actor?  Will Smith?  Denzel Washington?

## actor ... person_id
##			movie_id ... details
##			name ... count movies

# https://rvest.tidyverse.org/index.html

## Denzel Washington [nm0000243] vs Will Smith [nm0000226]

## https://www.imdb.com/filmosearch/?explore=title_type&role=nm0000243&ref_=filmo_ref_typ&sort=num_votes,desc&mode=detail&page=1&title_type=movie

# R , javascript, php, (c/c++)

# imdb ... 


grabFilmInfoFromFilmsPage = function(page)
{
  # 50 elements
  # # title = id = rank = year = rating = minutes = genre = votes = metascore = desc = millions
  
  movies = page %>%
    html_nodes(".mode-detail");
  
  
  
  pagecount = length(movies);
  
  result = data.frame( 			matrix(ncol = 11,nrow = pagecount) );
  # a matrix-type form with lots of NA values ...
  
  colnames(result) = c("rank", "title", "ttid", "year", "rated", "minutes", "genre", "ratings", "metacritic", "votes", "millions"); 
  
  
  for(i in 1:pagecount)
  {
    movie = movies[i];
    
    rank = movie %>%
      html_node(".lister-item-index") %>%
      html_text() %>%
      as.numeric();
    result$rank[i] = rank;
    
    title = movie %>%
      html_node(".lister-item-header a") %>%
      html_text();
    result$title[i] = title;
    
    ttid = movie %>%
      html_node(".lister-item-header a") %>%
      html_attr("href");
    
    temp = strsplit(ttid,"/",fixed=T);
    ttid = temp[[1]][3];
    result$ttid[i] = ttid;
    
    year = movie %>%
      html_node(".lister-item-year") %>%
      html_text();
    year = cleanupYear(year);
    result$year[i] = year;
    
    rated = movie %>%
      html_node(".certificate") %>%
      html_text();
    result$rated[i] = rated;
    
    minutes = movie %>%
      html_node(".runtime") %>%
      html_text();
    minutes = cleanupMinutes(minutes);
    result$minutes[i] = minutes;		
    
    genre = movie %>%
      html_node(".genre") %>%
      html_text();
    genre = str_trim(genre);
    result$genre[i] = genre;
    
    ratings = movie %>%
      html_node("div .rating-list") %>%
      html_attr("title");
    temp = strsplit(ratings,"/",fixed=T);
    temp = gsub("Users rated this","",temp[[1]][1],fixed=T);	
    temp = str_trim(temp);
    ratings = as.numeric(temp);
    result$ratings[i] = ratings;
    
    metacritic = movie %>%
      html_node(".ratings-metascore span") %>%
      html_text();
    metacritic = as.numeric(str_trim(metacritic));
    result$metacritic[i] = metacritic;
    
    # para ... +5 EASTER EGG ...
    
    info = movie %>%
      html_nodes(".lister-item-content p span") %>%
      html_text();
    
    votes = as.numeric(gsub(",","",info[8],fixed=T));
    result$votes[i] = votes;
    
    millions = cleanupMillions(info[11]);
    result$millions[i] = millions;			
  }
  
  #str(result);
  
  result;
}







cleanupMillions = function(millions)
{
  millions = gsub('$','',millions, fixed=T);
  millions = gsub('M','',millions, fixed=T);
  
  millions = as.numeric(millions);
  millions;
}

cleanupMinutes = function(minutes)
{
  minutes = gsub('min','',minutes, fixed=T);
  
  minutes = as.numeric(minutes);
  minutes;
}

cleanupYear = function(year)
{
  year = gsub('(','',year, fixed=T);
  year = gsub(')','',year, fixed=T);
  year = gsub('I','',year, fixed=T);
  year = as.numeric(year);
  year;
}

grabNameFromFilmsPage = function(page)
{
  name = page %>%
    html_node(".header") %>%
    html_text();
  
  name = gsub("Most Rated Feature Films With","",name,fixed=T);
  name = str_trim(name);
  
  name;
}


grabFilmCountFromFilmsPage = function(page)
{
  totalcount = page %>%
    html_nodes(".desc") %>%
    html_text();
  
  temp = strsplit(totalcount,"of",fixed=T);
  temp2 = strsplit(temp[[1]][2],"titles", fixed=T);
  
  totalcount = str_trim(temp2[[1]][1]);
  totalcount = as.numeric(totalcount);
  
  temp2 = strsplit(temp[[1]][1],"to", fixed=T);
  
  pagecount = str_trim(temp2[[1]][2]);
  pagecount = as.numeric(pagecount);
  
  result = list();
  
  result$totalcount = totalcount;
  result$pagecount = pagecount;
  
  result;
}




grabFilmsForPerson = function(nmid)
{
  url = paste("https://www.imdb.com/filmosearch/?explore=title_type&role=",nmid,"&ref_=filmo_ref_typ&sort=num_votes,desc&mode=detail&page=1&title_type=movie", sep="");
  
  page1 = read_html(url);
  result = list();
  ## useful for other data purposes
  result$nmid = nmid;
  
  ## name of person
  result$name = grabNameFromFilmsPage(page1);
  result$countfilms = grabFilmCountFromFilmsPage(page1);
  
  result$movies.50 = grabFilmInfoFromFilmsPage(page1);
  
  result;
}


