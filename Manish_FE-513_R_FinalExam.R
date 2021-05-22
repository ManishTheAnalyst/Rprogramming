#Importing necessary libraries

library(Quandl)
library(ggplot2)
library(gridExtra)
library(rlang)


#Defining a function
stock_func <- function(stock_ticker,start_time,end_time,time_interval)
{
  #Inserting Quandl API key
  Quandl.api_key("y1njHMQ73hS5zmUcA5Q1")
  
  #Getting the stock data
  DIS_stock <- Quandl(stock_ticker,collapse= "daily", start_date= start_time, end_date= end_time)
  head(DIS_stock)
  
  #Getting the adjusted close price
  df <- DIS_stock[12]
  
  #Converting from list to numeric
  df1<- as.numeric(unlist(df))
  
  #Preparing the dataframe to be unaffected by column numbers and to be transformed to 2-D easily
  length(df1) <- prod(dim(matrix(df1, ncol = time_interval)))
  
  #Transforming to 2-D dataframe
  origData <- matrix(df1, ncol = time_interval, byrow = TRUE)
  origData
 
  #Return origData
  return(origData)
  
}

#User must enter the inputs here
stock_func("EOD/DIS","2017-01-01","2017-12-25",12)


#Replacing the NA values with ZERO
origData[is.na(origData)] <- 0
origData

#Calculating Standard Deviation of each column
x <- data.frame(apply(origData,2,sd))


#Calculating Mean of each column
y <- data.frame(apply(origData,2,mean))


#Combing to form single dataframe
z<- cbind(x,y)


#Nicely formatted table
colnames(z) <- c("Standard_Deviation", "Mean")
z
grid.table(z)

#Scatter Plot of Column vs Mean
ggplot(aes(x = as.numeric(row.names(z)), y = Mean), data = z) +
  geom_point()+ theme_bw()+ xlab("Columns")+ylab("Mean")+
  scale_x_continuous(breaks = seq(0, 100, 1))

#Scatter Plot of Column vs Standard Deviation
ggplot(aes(x = as.numeric(row.names(z)), y = Standard_Deviation), data = z) +
  geom_point()+ theme_bw()+ xlab("Columns")+ylab("Standard Deviation")+
  scale_x_continuous(breaks = seq(0, 100, 1))


