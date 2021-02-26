library(quantmod)
library(rugarch)
library(rmgarch)
library(dplyr)
library(writexl)

startDate = as.Date("2015-01-01")#startdate for the dataset

endDate = as.Date("2018-01-04") #enddate for the dataset

#extracting stock data for IBM
getSymbols("IBM", from = startDate, to = endDate)

#saving the table into a dataframe
IBM.df <- as.data.frame(IBM) 

#to export the extracted data in an excel
write_xlsx(IBM.df,"C:/Users/NGDRS-1/Documents/R/Garch_data.xlsx")
head(IBM)
IBMClose<-IBM$IBM.Close
IBMClose

#creating object for ugarchspec
ug_spec = ugarchspec()
ug_spec

#fit the model using daily return values
ugfit = ugarchfit(spec = ug_spec, data = IBMClose)
ugfit

#identifying the values of the constants alpha, beta, omega
ugfit@fit$coef

#forecasting conditional volatality and stock price for the next 10 days
ugb <- ugarchboot(ugfit,n.ahead = 50, method=c("Partial","Full")[1])
ugb
plot(ugb, which =2)

#storing the sigma and stock value separately in different dataframes
sig = sigma(ugb@forc)
ser = fitted(ugb@forc)
IBMsigma = cbind(t(as.data.frame(ugb, which = "sigma", type = "summary")),  sig)
IBMseries = cbind(t(as.data.frame(ugb, which = "series", type = "summary")), ser)
se<-IBMseries[, (6)]
sg<-IBMsigma[, (6)]
IBMFore.df <- as.data.frame(se) 
IBMsig.df <- as.data.frame(sg) 

#forecasted stock value for next 10 days
IBMFore.df

#forecasted sigma value for next 10 days
IBMsig.df

#sigma values for the existing data
b<-sigma(ugfit)
b.df<-as.data.frame(b)

#storing last 10 sigma values for analysis
b10<-tail(b.df, n=757)


#merge the sigma values of existing data and forecasted data
names(b10)[names(b10) == "V1"] <- "sigma"
names(IBMsig.df)[names(IBMsig.df) == "sg"] <- "sigma"
IBMVolatility <- bind_rows(b10,IBMsig.df)

#plot the volatility of the existing data and forecasted data
plot(IBMVolatility$sigma,type = "l")

# save the estimated squared residuals
ug_res <- (ugfit@fit$residuals)^2

#plot the residual graph
plot(ug_res)


