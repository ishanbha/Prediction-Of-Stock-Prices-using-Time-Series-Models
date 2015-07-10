# =====================================================================
# CSE587
# Author: Ishan Bhatt
# Email: ishanbha@buffalo.edu
# =====================================================================

# need to install the following two packages in CCR(at least)
# install.packages("forecast")
# install.packages("fpp")
# data path /gpfs/courses/cse587/spring2015/data/hw2/data

library(forecast)
library(fpp)
library(R.utils)
# need to read the stocklist, and loop all files
stocklist = read.table("/gpfs/courses/cse587/spring2015/data/hw2/stocklist.txt")
stocklist = t(stocklist)
error = c()
name = c()
for(j in 1:length(stocklist)){
  if(stocklist[j]=="ULTAInc.") {
    stocklist[j] = "ULTA"
  }
  if(stocklist[j]=="JASOLtd."){
    stocklist[j] = "JASO"
  } 
  filename = paste("/gpfs/courses/cse587/spring2015/data/hw2/data/",stocklist[j],".csv",sep="")
  # if file is not empty and has 755 lines

  if(file.info(filename)[1]>0 && countLines(filename)==755) {   
 
    # read one csv file into variable (DO NOT EDIT)
    textData=read.csv(file=filename, header=T)
    
    # convert txt data to time-series data, in day unit (DO NOT EDIT)
    tsData = ts(rev(textData$Adj.Close),start=c(2012, 1),frequency=365)
    
    # define train data (DO NOT EDIT)
    trainData = window(tsData, end=c(2014,14))
    
    # define test data (DO NOT EDIT)
    testData = window(tsData, start=c(2014,15))
    
    # MAE row vector (DO NOT EDIT)
    MAE_arima = matrix(NA,1,length(testData))
    # apply ARIMA model (DO NOT EDIT)
    fitData_arima = auto.arima(trainData)
    
    # apply forecast(DO NOT EDIT)
    forecastData_arima = forecast(fitData_arima, h=length(testData))
    # calculate Mean Absolute Error 
    for(i in 1:length(testData))
    {
      MAE_arima[1,i] = abs(forecastData_arima$mean[i] - testData[i])
    }
    error = c(error,sum(MAE_arima[1,1:10]))
    name = c(name, stocklist[j])
  }
}
arima_list = data.frame(name, error)
arima_list = arima_list[order(error),]
rownames(arima_list) = NULL
#print results
print("The top 10 minimum sum of MAE using Arima are:")
print(arima_list[1:10,])
# plot graph
jpeg("/gpfs/courses/cse587/spring2015/students/ishanbha/hw2/arima.jpg")
plot(arima_list$error[1:10],col="blue",ylab="MAE", xlab="Stocks",xaxt="n")
lines(arima_list$error[1:10], col="red")
title(main="Arima Model")
axis(1,at=1:10,labels=arima_list$name[1:10],las=2)
dev.off()
