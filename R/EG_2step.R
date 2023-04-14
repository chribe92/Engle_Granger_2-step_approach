



#' 
#' 
#' ==================================================
#' Engle and Granger 2-step approach function!
#' ==================================================
#' 
#' 
#' 
#' 
#' Employing the Engle and Granger 2-step approach
#' on every pairwise combination of time series. 
#' 
#' 
#' INFO ----
#' 
#' 1. Data must be a "data.frame", or "tibble".
#' 2. The data cannot contain a "date" column, thus you would have to e.g., "data[,-1]"
#' 
#' 
#' 
#' 
#' WHAT THE FUNCTION DOES ----
#' 
#' 1. The function Estimate a simple regression model between pairwise variables to obtain the residuals.
#' 2. Employing Augmented Dickey-Fuller (ADF) on every residuals from the aforementioned regression model


# A function that loades or downloads library ----
using <-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

# Loading the libraries with the 'using' function
using("urca",
      "forecast",
      "tidyverse",
      "vars"
)


# Creating the Engle and Granger 2-step approach function ----
EG_2step <- function(data = data){
  
  # Converting data to data.frame
  data <- as.data.frame(data)
  
  # Initialising empty data.frame to store results
  table <- data.frame(pair = character(), pvalue = numeric(), statistic =numeric())
  
  # loop over all pairs of ith and jth time series
  for (i in 1:(ncol(data)-1)) {
    for (j in (i+1):ncol(data)) {
      # Fit linear regression model on ith and jth pair of time series
      reg <- lm(data[[i]] ~ data[[j]])
      
      # extract the residuals from the linear regression model
      resid <- residuals(reg)
      # Employ the adf test
      adf <- adf.test(resid)
      # Extracting p-value and test statistic 
      pvalue <- adf[[4]]
      statistic <- adf[[1]]
      
      # add result to table
      pair <- paste(colnames(data[,-1])[i], colnames(data[,-1])[j], sep=" & ")
      table <- table %>% add_row(pair=pair, pvalue=pvalue, statistic=statistic)  
    }
  }
  return(table)
}


