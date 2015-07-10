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
names = c()
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
    MAE_lm = matrix(NA,1,length(testData))
    # apply Linear  model
    fitData_lm = tslm(trainData ~ trend)
    
    # apply forecast(DO NOT EDIT)
    forecastData_lm = forecast(fitData_lm, h=length(testData))
    # calculate mean absolute error
    for(i in 1:length(testData))
    {
      MAE_lm[1,i] = abs(forecastData_lm$mean[i] - testData[i])
    }
    error = c(error,sum(MAE_lm[1,1:10]))
    names = c(names, stocklist[j])
  }
}
lm_list = data.frame(names,error)
lm_list = lm_list[order(error),]
rownames(lm_list)=NULL

# print results
print("The top 10 minimum sum of MAE using linear Regression are:")
print(lm_list[1:10,])
# plot graph
jpeg("/gpfs/courses/cse587/spring2015/students/ishanbha/hw2/lm.jpg")
plot(lm_list$error[1:10],col="blue",ylab="MAE", xlab="Stocks",xaxt="n")
lines(lm_list$error[1:10], col="red")
title(main="Linear Regression Model")
axis(1,at=1:10,labels=lm_list$name[1:10],las=2)
dev.off()
