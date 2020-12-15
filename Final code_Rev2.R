#------Visualization to check the highest no of cases and deaths worldwide------------------------------------------------------------------------------Visualization 

library(forecast)
library(zoo)
library(readxl)

# Set working directory for locating files.
path = setwd("C:/Users/Rao/Documents/Career/MSBA/BAN 673/Project")

# Create data frame for monthly data
covid.data <- read_excel("COVID-19.xlsx",sheet="Monthly")
covid.data

## Making countrywise ts
Brazil <- covid.data[c(1:12),]
France <- covid.data[c(13:24),]
India <- covid.data[c(25:36),]
Russia <- covid.data[c(37:48),]
Spain <- covid.data[c(49:60),]
UK <- covid.data[c(61:72),]
USA <- covid.data[c(73:84),]

## Countrywise Timeseries for cases
Brazil.cases.ts <- ts(Brazil$cases, start = c(2020, 1), end = c(2020, 12), freq = 12)
France.cases.ts <- ts(France$cases, start = c(2020, 1), end = c(2020, 12), freq = 12)
India.cases.ts <- ts(India$cases, start = c(2020, 1), end = c(2020, 12), freq = 12)
Russia.cases.ts <- ts(Russia$cases, start = c(2020, 1), end = c(2020, 12), freq = 12)
Spain.cases.ts <- ts(Spain$cases, start = c(2020, 1), end = c(2020, 12), freq = 12)
UK.cases.ts <- ts(UK$cases, start = c(2020, 1), end = c(2020, 12), freq = 12)
USA.cases.ts <- ts(USA$cases, start = c(2020, 1), end = c(2020, 12), freq = 12)

## plotting countrywise case
par(mfcol = c(1,1), xpd=TRUE)
plot(Brazil.cases.ts,main="Countrywise cases",col="blue", xlab="Time", ylab="Cases", ylim=c(0, 3000000), bty="l")
#axis(1, at = seq(as.Date("2020-01-31"), as.Date("2020-12-31"), by = "month"))
lines(France.cases.ts, col="red",lty=2)
lines(India.cases.ts, col="darkgreen", lty=3)
lines(Russia.cases.ts, col="black", lty=4)
lines(Spain.cases.ts, col="orange", lty=5)
lines(UK.cases.ts, col="magenta", lty=6)
lines(USA.cases.ts, col="brown", lty=7)
legend("topleft", legend=c("Brazil","France","India","Russia","Spain","UK","USA"), col=c("blue","red","darkgreen","black","orange","magenta","brown"), lty = c(1,2,3,4,5,6,7))

## Countrywise Timeseries for deaths
Brazil.death.ts <- ts(Brazil$deaths, start = c(2020, 1), end = c(2020, 12), freq = 12)
France.death.ts <- ts(France$deaths, start = c(2020, 1), end = c(2020, 12), freq = 12)
India.death.ts <- ts(India$deaths, start = c(2020, 1), end = c(2020, 12), freq = 12)
Russia.death.ts <- ts(Russia$deaths, start = c(2020, 1), end = c(2020, 12), freq = 12)
Spain.death.ts <- ts(Spain$deaths, start = c(2020, 1), end = c(2020, 12), freq = 12)
UK.death.ts <- ts(UK$deaths, start = c(2020, 1), end = c(2020, 12), freq = 12)
USA.death.ts <- ts(USA$deaths, start = c(2020, 1), end = c(2020, 12), freq = 12)

## plotting countrywise deaths
par(mfcol = c(1,1), xpd=TRUE)
plot(Brazil.death.ts,main="Countrywise deaths",col="blue", xlab="Time", ylab="Deaths", ylim=c(0, 80000), bty="l")
#axis(1, at = seq(as.Date("2020-01-31"), as.Date("2020-12-31"), by = "month"))
lines(France.death.ts, col="red",lty=2)
lines(India.death.ts, col="darkgreen", lty=3)
lines(Russia.death.ts, col="black", lty=4)
lines(Spain.death.ts, col="orange", lty=5)
lines(UK.death.ts, col="magenta", lty=6)
lines(USA.death.ts, col="brown", lty=7)
legend("topright", legend=c("Brazil","France","India","Russia","Spain","UK","USA"), col=c("blue","red","darkgreen","black","orange","magenta","brown"), lty = c(1,2,3,4,5,6,7))



#------------------------------------------------------------------------------
# data frame for Weekly Data for USA
covid.data_weekly <- read_excel("COVID-19.xlsx",sheet="Weekly")
covid.data_weekly

# Creating time series data set for Cases and Death
cases_weekly.ts <- ts(covid.data_weekly$Cases, 
                   start = c(2020, 1), end = c(2020, 46), freq = 52)

death_weekly.ts <- ts(covid.data_weekly$Death, 
                      start = c(2020, 1), end = c(2020, 46), freq = 52)

## Time series Plot for Cases & Death 
plot(cases_weekly.ts, 
     xlab = "Time", ylab = "Cases ", 
     ylim = c(0, 2000000), main = "USA_Cases", col = "blue")

plot(death_weekly.ts, 
     xlab = "Time", ylab = "Deaths ", 
     ylim = c(0, 20000), main = "USA_Deaths", col = "Green")



#Apply the Acf() function to identify possible time series components. Provide in the report
#the autocorrelation chart and explain the time series components existing in the historical
#data. 

autocor <- Acf(cases_weekly.ts, lag.max = 52, main = "Autocorrelation for weekly Confirmed Cases in US")

autocor <- Acf(death_weekly.ts, lag.max = 52, main = "Autocorrelation for No of deaths in US")



# Partition the dataset in to training and validation for cases in US
nValid_cases <- 14
nTrain_cases <- length(cases_weekly.ts) - nValid_cases
train.ts_cases <- window(cases_weekly.ts, start = c(2020, 1), end = c(2020, nTrain_cases))
valid.ts_cases <- window(cases_weekly.ts, start = c(2020, nTrain_cases + 1), 
                   end = c(2020, nTrain_cases + nValid_cases))

# Partition the dataset in to training and validation for deaths in US
nValid_deaths <- 14
nTrain_deaths <- length(death_weekly.ts) - nValid_deaths
train.ts_deaths <- window(death_weekly.ts, start = c(2020, 1), end = c(2020, nTrain_deaths))
valid.ts_deaths <- window(death_weekly.ts, start = c(2020, nTrain_deaths + 1), 
                   end = c(2020, nTrain_deaths + nValid_deaths))

#--------------------------------Cases----------------------------------------------

##Naive Model for cases in US for partition data
traincases.naive.pred <- naive(train.ts_cases, h = nValid_cases)

# Regression model with linear trend for no of cases 
# Use tslm() function (time series linear model) to create regression model with 
# linear trend.
train.lin_cases <- tslm(train.ts_cases ~ trend)

# See summary of linear trend model and asociated parameters.
summary(train.lin_cases)

# Apply forecast() function to make forecast for validation period.
train.lin_cases.pred <- forecast(train.lin_cases, h = nValid_cases, level = 0)


# Holt's model with optimal smoothing parameters for cases 
# Use ets() function with model = "AAN", i.e., additive error(A), 
# additive trend (A), & no seasonality (N). 
train.h.AAN.opt_cases <- ets(train.ts_cases,model="AAN")
train.h.AAN.opt_cases

# Use forecast() function to make predictions using this HW model for 
# validation period (nValid). 
# Show predictions in tabular format.
train.h.AAN.opt_cases.pred <- forecast(train.h.AAN.opt_cases, h = nValid_cases, level = 0)
train.h.AAN.opt_cases.pred


#  Arima Model (2,1,1)

train.arima_cases <- Arima(train.ts_cases, order = c(2,1,1)) 
summary(train.arima_cases)

# forecast
train.arima_cases.pred <- forecast(train.arima_cases,h=nValid_cases,level=0)
train.arima_cases.pred

Acf(train.arima_cases$residuals, lag.max = 52, 
    main = "Autocorrelations of ARIMA Model Residuals")

# Auto ARIMA Model

train.auto_arima_cases <- auto.arima(train.ts_cases)
summary(train.auto_arima_cases)

# forecast
train.auto_arima_cases.pred <- forecast(train.auto_arima_cases,h=nValid_cases,level=0)
train.auto_arima_cases.pred

Acf(train.auto_arima_cases$residuals, lag.max = 52, 
    main = "Autocorrelations of ARIMA Model Residuals for cases")



# Accuracy FOR Naive and Seasonal Naive Forecasts
# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(traincases.naive.pred, valid.ts_cases), 3)
round(accuracy(train.lin_cases.pred, valid.ts_cases), 3)
round(accuracy(train.h.AAN.opt_cases.pred, valid.ts_cases), 3)
round(accuracy(train.arima_cases.pred, valid.ts_cases), 3)
round(accuracy(train.auto_arima_cases.pred, valid.ts_cases), 3)



#-------------------------------Deaths------------------------------------------

### Naive model for Deaths in US
traindeaths.naive.pred <- naive(train.ts_deaths, h = nValid_deaths)

# Regression model with linear trend for no of deaths 
# Use tslm() function (time series linear model) to create regression model with 
# linear trend.
train.lin_deaths <- tslm(train.ts_deaths ~ trend)

# See summary of linear trend model and asociated parameters.
summary(train.lin_deaths)

# Apply forecast() function to make forecast for validation period.
train.lin_deaths.pred <- forecast(train.lin_deaths, h = nValid_deaths, level = 0)


# Holt's model with optimal smoothing parameters for deaths  
# Use ets() function with model = "AAN", i.e., additive error(A), 
# additive trend (A), & no seasonality (N). 
train.h.AAN.opt_deaths <- ets(train.ts_deaths, model = "AAN")
train.h.AAN.opt_deaths

# Use forecast() function to make predictions using this HW model for 
# validation period (nValid). 
# Show predictions in tabular format.
train.h.AAN.opt_death.pred <- forecast(train.h.AAN.opt_deaths, h = nValid_deaths, level = 0)
train.h.AAN.opt_death.pred

#  Arima Model (2,1,1)

train.arima_death <- Arima(train.ts_deaths, order = c(2,1,1)) 
summary(train.arima_death)

# forecast
train.arima_death.pred <- forecast(train.arima_death,h=nValid_deaths,level=0)
train.arima_death.pred

Acf(train.arima_death$residuals, lag.max = 52, 
    main = "Autocorrelations of ARIMA Model Residuals for deaths")

# Auto ARIMA Model

train.auto_arima_death <- auto.arima(train.ts_deaths)
summary(train.auto_arima_death)

# forecast
train.auto_arima_death.pred <- forecast(train.auto_arima_death,h=nValid_deaths,level=0)
train.auto_arima_death.pred

Acf(train.auto_arima_death$residuals, lag.max = 52, 
    main = "Autocorrelations of ARIMA Model Residuals for deaths")


# Accuracy FOR Naive and Seasonal Naive Forecasts
# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(traindeaths.naive.pred, valid.ts_deaths), 3)
round(accuracy(train.lin_deaths.pred, valid.ts_deaths), 3)
round(accuracy(train.h.AAN.opt_death.pred, valid.ts_deaths), 3)
round(accuracy(train.arima_death.pred, valid.ts_deaths), 3)
round(accuracy(train.auto_arima_death.pred, valid.ts_deaths), 3)

#------------------------MODELS ON ENTIRE DATASET-CASES------------------------------------------


### Naive model for Cases in US using Entire Dataset
Cases.naive.pred <- naive(cases_weekly.ts, h = 7)
Cases.naive.pred


# Regression model with linear trend for no of cases 
# Use tslm() function (time series linear model) to create regression model with 
# linear trend.
lin_cases <- tslm(cases_weekly.ts ~ trend)

# See summary of linear trend model and asociated parameters.
summary(lin_cases )

# Apply forecast() function to make forecast for entire Dataset for coming 7 weeks.
lin_cases.pred <- forecast(lin_cases, h = 7, level = 0)


# Holt's model with optimal smoothing parameters for cases on entire dataset
# Use ets() function with model = "AAN", i.e., additive error(A), 
# additive trend (A), & no seasonality (N). 
h.AAN.opt_cases <- ets(cases_weekly.ts, model = "AAN")
h.AAN.opt_cases

# Use forecast() function to make predictions using this HW model for 
#entire dataset for coming 7 weeks. 
# Show predictions in tabular format.
h.AAN.opt_cases.pred <- forecast(h.AAN.opt_cases, h = 7, level = 0)
h.AAN.opt_cases.pred


#  Arima Model (2,1,1)

arima_cases <- Arima(cases_weekly.ts, order = c(2,1,1)) 
summary(arima_cases)

# forecast for coming 7 weeks
arima_cases.pred <- forecast(arima_cases,h=7,level=0)
arima_cases.pred

Acf(arima_cases.pred$residuals, lag.max = 52, 
    main = "Autocorrelations of ARIMA Model Residuals-Entire Dataset")

# Auto ARIMA Model

auto_arima_cases <- auto.arima(cases_weekly.ts)
summary(auto_arima_cases)

# forecast for coming 7 weeks
auto_arima_cases.pred <- forecast(auto_arima_cases,h=7,level=0)
auto_arima_cases.pred

Acf(auto_arima_cases$residuals, lag.max = 52, 
    main = "Autocorrelations of Auto ARIMA Model Residuals for cases-Entire Dataset")



# Accuracy FOR Naive and Seasonal Naive Forecasts
# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.

round(accuracy(Cases.naive.pred$fitted, cases_weekly.ts), 3)
round(accuracy(lin_cases.pred$fitted, cases_weekly.ts), 3)
round(accuracy(h.AAN.opt_cases.pred$fitted, cases_weekly.ts), 3)
round(accuracy(arima_cases.pred$fitted, cases_weekly.ts), 3)
round(accuracy(auto_arima_cases.pred$fitted, cases_weekly.ts), 3)


#------------------------MODELS ON ENTIRE DATASET-DEATH------------------------------------------


### Naive model for Cases in US using Entire Dataset
Deaths.naive.pred <- naive(death_weekly.ts, h = 7)
Deaths.naive.pred

# Regression model with linear trend for no of deaths 
# Use tslm() function (time series linear model) to create regression model with 
# linear trend.
lin_deaths <- tslm(death_weekly.ts  ~ trend)

# See summary of linear trend model and asociated parameters.
summary(lin_deaths )

# Apply forecast() function to make forecast for entire dataset for coming 7 weeks.
lin_deaths.pred <- forecast(lin_deaths, h = 7, level = 0)


# Holt's model with optimal smoothing parameters for cases on entire dataset 
# Use ets() function with model = "AAN", i.e., additive error(A), 
# additive trend (A), & no seasonality (N). 
h.AAN.opt_deaths <- ets(death_weekly.ts, model = "AAN")
h.AAN.opt_deaths

# Use forecast() function to make predictions using this HW model for 
# entire dataset for coming 7 weeks. 
# Show predictions in tabular format.
h.AAN.opt_deaths.pred <- forecast(h.AAN.opt_deaths, h = 7, level = 0)
h.AAN.opt_deaths.pred


#  Arima Model (2,1,1)

arima_deaths <- Arima(death_weekly.ts , order = c(2,1,1)) 
summary(arima_deaths)

# forecast for coming 7 weeks
arima_deaths.pred <- forecast(arima_deaths,h=7,level=0)
arima_deaths.pred

#Acf plot for entire Dataset for coming 7 weeks
Acf(arima_deaths.pred$residuals, lag.max = 52, 
    main = "Autocorrelations of ARIMA Model Residuals-Entire Dataset")

# Auto ARIMA Model

auto_arima_deaths <- auto.arima(death_weekly.ts )
summary(auto_arima_deaths)

# forecast for coming 7 weeks
auto_arima_deaths.pred <- forecast(auto_arima_deaths,h=7,level=0)
auto_arima_deaths.pred

Acf(auto_arima_deaths$residuals, lag.max = 52, 
    main = "Autocorrelations of  Auto ARIMA Model Residuals for Deaths-Entire Dataset")

# Accuracy FOR Naive and Seasonal Naive Forecasts
# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.

round(accuracy(Deaths.naive.pred$fitted, death_weekly.ts ), 3)
round(accuracy(lin_deaths.pred$fitted, death_weekly.ts ), 3)
round(accuracy(h.AAN.opt_deaths.pred$fitted, death_weekly.ts ), 3)
round(accuracy(arima_deaths.pred$fitted, death_weekly.ts ), 3)
round(accuracy(auto_arima_deaths.pred$fitted, death_weekly.ts ), 3)
