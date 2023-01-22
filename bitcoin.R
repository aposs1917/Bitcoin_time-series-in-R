library(tidyverse)
library(ggplot2)
library(ggpubr)
library(tsibble)

df=read.csv("BTC-USD.csv")

df %>% is.na() %>% sum
### Format date column as date
df$Date=df$Date %>% as.Date(format="%Y-%m-%d")

df=df %>% as_tsibble(index=Date)

##### Autocorrelation and partial autocorrelation

lag.plot(df$Close,lags=4)

acf(df$Close)

pacf(df$Close)
pacf(df$Close,plot=FALSE)

######### Cast into terms of growth rates

dfperc=df %>% mutate_at(vars(2,3,4,5,6,7),.funs=function(x) (x-lag(x))/lag(x))

dfperc$Close %>% plot(type="l")
### seems like white noise with periods of increased volatility

##### Distirbution of percentace closing price change
dfperc$Close %>% na.omit %>% summary %>% 
  append(sd(dfperc$Close,na.rm=T))

par(mfrow=c(1,2))
dfperc$Close %>% na.omit %>% hist(main="")
dfperc$Close %>% na.omit %>% density %>% plot(main="")
par(mfrow=c(1,1))
qqnorm(y=dfperc$Close)
e1071::kurtosis(dfperc$Close,na.rm=T)

#Overall the distirbution of daily returns is leptokurtic, as shown by the density plot and the estimated kurtosis


#####Distirbution of daily returns by period
#find a suitable period split based on available data
dfperc$Close %>% length # Observations are length of series - 1 , due to taking differences
df_periods=dfperc %>% na.omit
df_periods$period=rep(c(1:39),each=75)

Price_periods=df_periods %>% as.tibble %>% group_by(period) %>% 
  summarize(mean=mean(Close),std=sd(Close))

Price_periods$cv=Price_periods$std/Price_periods$mean  #estimate coefficient of variation for each period

Price_periods$cv %>% plot(type="l")
Price_periods$mean %>% plot(type="l")
Price_periods$std %>% plot(type="l")
#There is an outlier in the coefficient of variation
Price_periods[which(Price_periods$cv==max(Price_periods$cv)),]
Price_periods %>% arrange(desc(cv))
#Lets plot the coefficient of variation boxplot without the aforementioned outlier
ggplot(data=Price_periods %>% filter(period!=22))+
  geom_line(aes(x=period,y=cv))

Price_periods$mean %>% boxplot
Price_periods$std %>% boxplot
Price_periods[which(Price_periods$cv!=max(Price_periods$cv)),"cv"] %>% boxplot
Price_periods[which(!(Price_periods$period %in% c(7,22))),"cv"] %>% boxplot

#Ridgelines  over time

ggplot(df_periods, aes(x =Close, y =as.factor(period))) + 
  ggridges::geom_density_ridges()+
  geom_point(data=Price_periods,aes(x=mean,y=period),col="red")+coord_flip()


ggplot(df_periods, aes(x =Close, y =as.factor(period))) + 
  ggridges::geom_density_ridges()+
  geom_point(data=Price_periods,aes(x=std,y=period),col="red")+coord_flip()



##### Seasonal adjustment

forecast::ggsubseriesplot(ts(df$Close,frequency=4)) #Check quarterly seasonality
forecast::ggsubseriesplot(ts(df$Close,frequency=12)) #check monthly seasonality
forecast::ggsubseriesplot(ts(df$Close,frequency=7)) #check daily seasonality



###Weekly rolling average
df$Close %>% plot(type="p",pch=21,bg="black",cex=0.6)
lines(zoo::rollmean(df$Close,k=7),col="red",lwd=4)
(
  df$Close[7:length(df$Close)] -zoo::rollmean(df$Close,k=7)
) %>% plot(type="l")  #plot after removing 7day moving average
###Monthly rolling average

df$Close %>% plot(type="p",pch=21,bg="black",cex=0.6)
lines(zoo::rollmean(df$Close,k=30),col="red",lwd=4)
(
  df$Close[30:length(df$Close)] -zoo::rollmean(df$Close,k=30)
) %>% plot(type="l") #plot after removing 30day moving average



decadd=ts(df$Close,frequency = 30) %>% decompose (type="additive")
decmult=ts(df$Close,frequency = 30) %>% decompose (type="multiplicative")

decadd %>% plot
decmult %>% plot


##### Lower frequency plots

dfperc %>% na.omit %>% index_by(Year_Month = ~ yearmonth(.)) %>% summarize(mean=mean(Close)) %>% plot(type="l")

dfperc %>%na.omit %>%  index_by(Year_week = ~ yearweek(.)) %>%  summarize(mean=mean(Close)) %>% plot(type="l")

##### Fit an ARIMA model
model=forecast::auto.arima(df$Close)

model
#random walk

forecast::forecast(model, level=c(95), h=10*12) %>% plot

##### tidymodels

library(tidymodels)
library(modeltime)
library(rsample)
library(timetk)

tsplits=time_series_split(data=df %>% as.tibble(),date_var=Date,assess=30,cumulative=TRUE)

###visualize train_test split 

tsplits %>%  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(Date,Close,.interactive = FALSE)


#list of possible models https://www.tidymodels.org/find/parsnip/

model_fit_naive=naive_reg() %>% 
  set_engine("naive") %>% 
  fit(Close~Date,training(tsplits))

show_engines("exp_smoothing")

model_fit_ets = exp_smoothing() %>%
  set_engine("ets") %>%
  fit(Close ~ Date, training(tsplits))

model_fit_arima = arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(Close ~ Date, training(tsplits))

model_fit_prophet=prophet_reg() %>%
  set_engine("prophet", quarterly.seasonality = TRUE) %>%
  fit(Close ~ Date, training(tsplits))


#####Comparison with model time
library(modeltime)

model_table =modeltime_table(
  model_fit_naive,
  model_fit_arima,
  model_fit_ets,
  model_fit_prophet
)

model_table

#Calibration

calibration_table=model_table %>%
  modeltime_calibrate(testing(tsplits))

calibration_table

#forecasting


calibration_table %>%
  modeltime_forecast(actual_data = df) %>%
  plot_modeltime_forecast(.interactive = FALSE)


#See accuracy of each model

calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

calibration_table %>% 
  modeltime_forecast(actual_data = df %>% as.tibble) %>%filter(.index>"2021-01-01") %>% 
  plot_modeltime_forecast(.interactive = T)


#####Machine learning models
df
training(tsplits)
###Preprocess
rspec=recipe(Close ~ Date, training(tsplits)) %>% 
  step_timeseries_signature(Date) %>% step_fourier(Date,period=365,K=6) %>% 
  step_rm(contains("am.pm"), contains("hour"), contains("minute"),
          contains("second"), contains("xts"),contains("lbl"))
  
rspec%>% prep() %>% juice() %>% names

#Elastic NET model

model_spec_glmnet=linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

workflow_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(rspec %>% step_rm(Date)) %>%
  fit(training(tsplits))

model_spec_prophet_boost <- prophet_boost() %>%
  set_engine("prophet_xgboost", quarterly.seasonality = TRUE) 

workflow_fit_prophet_boost <- workflow() %>%
  add_model(model_spec_prophet_boost) %>%
  add_recipe(rspec) %>%
  fit(training(tsplits))

##### Assesing ml models

model_table =modeltime_table(
  workflow_fit_glmnet,
  workflow_fit_prophet_boost
)

calibration_table <- model_table %>%
  modeltime_calibrate(testing(tsplits))
calibration_table



calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

calibration_table %>% 
  modeltime_forecast(actual_data = df) %>%filter(.index>"2021-01-01") %>% 
  plot_modeltime_forecast(.interactive = T)
#Overall the ets model seems to give us better predictions, followerd by prophet with xg boos