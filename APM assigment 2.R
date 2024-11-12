data<- read.csv('Air_pollution_assignment.csv')
data$Date2 <- as.Date(data$Date)
str(data)

#plot the data
plot(data$Date2,data$BlackCarbon,type = "l",col = 2,ylim = c(0, 180),
     xlab = "Year",
     ylab = "Values")
lines(data$Date2, data$Carbon_Monoxide,type = "l", col = 3)
lines(data$Date2, data$Nitric_Oxigen,type = "l",col = 4)
lines(data$Date2, data$Nitrogen_Dioxide,type = "l",col = 5)
lines(data$Date2, data$Other,type = "l",col = 6)
lines(data$Date2, data$Ozone,type = "l",col = 7)
lines(data$Date2, data$PM10,type = "l",col = 8)
lines(data$Date2, data$PM2.5,type = "l",col =9)
legend("topright", c("BlackCarbon", "Carbon_Monoxide", "Nitric_Oxigen",
                     'Nitrogen_Dioxide','Other','Ozone', 'PM10','PM2.5'),
       lty = 1,
       col = 2:9)
#plot ozone only:
data_ozen<- as.data.frame(cbind(date=data$Date, ozone=data$Ozone))
data_ozen$date <- as.Date(data_ozen$date)
data_ozen$ozone <- as.numeric(data_ozen$ozone)

plot(data_ozen$date, data_ozen$ozone,type = "l", xlab = 'Year', ylab='Ozone concentration')

#correllogram:
autoplot(acf(data_ozen))
autoplot(acf(data_ozen$ozone))

#split data into training and validating

data_ozen_valid<- data_ozen[c(157:209),] #validation data
data_ozen<- data_ozen[-c(157:209),] #training data

#detrend data using Harmonic model:
x <- data_ozen[,2]
n <- length(x)
t <- 1:n
data_ozen_harm<- data_ozen

# linear trend with harmonic seasonal component:
Z.fixed <- cbind(t, sin(2*pi*t/52), cos(2*pi*t/52))
data_ozen_harm$trend.fixed <- lm(x~Z.fixed)$fitted.values
data_ozen_harm$x.fixed <- x - data_ozen_harm$trend.fixed

#plot harmonic detrended data
ggplot(data_ozen_harm, aes(date, x.fixed)) +
  geom_line(color = "#d73027", alpha=0.7) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 year") +
  xlab("Date") + ylab("Ozone") +
  ggtitle("Harmonic model for De-trended data")

#check residuals 
model.season <- lm(x~sin(2*pi*t/52) + cos(2*pi*t/52))
residual.series <- x - model.season$fitted.values
summary(model.season)
#predict(model.season, n.ahead=53)

# Plotting
p2 <- autoplot(ts(residual.series), ylab = "Residual series")
p2acf <- autoplot(acf(residual.series, plot = FALSE))
p2pacf <- autoplot(pacf(residual.series, plot = FALSE))

grid.arrange(p2, p2acf, p2pacf, nrow=3)
grid.arrange(p2acf, p2pacf, nrow=2)

#fit an AR(2) model to our data:

model.ar <- arima(residual.series, order=c(2,0,0))

# Plotting
p3 <- autoplot(model.ar$residuals, ylab = "AR(2) residual series")
p3acf <- autoplot(acf(model.ar$residuals, plot = FALSE))
p3pacf <- autoplot(pacf(model.ar$residuals, plot = FALSE) )

grid.arrange(p3, p3acf, p3pacf, nrow=3)
grid.arrange(p3acf, p3pacf, nrow=2)

#removing trend my moving average process:

model.ma <- arima(residual.series, order=c(0,0,3))
model.ma
# Plotting
p <- autoplot(model.ma$residuals, main="MA(3) residual series")
pacf <- autoplot(acf(model.ma$residuals, plot = FALSE))
ppacf <- autoplot(pacf(model.ma$residuals, plot = FALSE))

grid.arrange(p, pacf, ppacf, nrow=3)
grid.arrange( pacf, ppacf, nrow=2)


#removing trend with ARMA process
model.arma <- arima(residual.series, order=c(1,0,1))


# Plotting
p <- autoplot(model.arma$residuals, ylab="ARMA(1,1) residual series", main="") + 
  geom_line( colour="#4a1486")
pacf <- autoplot(acf(model.arma$residuals, plot = FALSE))
ppacf <- autoplot(pacf(model.arma$residuals, plot = FALSE))
grid.arrange(p, pacf, ppacf, nrow=3)
grid.arrange(pacf, ppacf, nrow=2)


#removing trend by differenceing: 

x <- data_ozen[,2]

# we use function diff(x, lag) for the difference operator
diff.1 <- data.frame(d=diff(x, lag = 1, differences = 1),
                     ind=data_ozen$date[-1])

ggplot(diff.1, aes(y=d, ind)) +
  geom_line(color="#fc4e2a", alpha=0.4) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 year") +
  xlab("Time") + ylab("Lag 1 difference") +
  ggtitle("First order differences")


diff1 <-diff(x, differences = 1)
diff2 <-diff(x, differences = 2)
d1plot<- plot(diff1, type="l", plot=FALSE)
d2plot<- plot(diff2, type="l", plot=FALSE)

#1 difference is sufficient. 

p1<- autoplot(acf(diff1, plot=FALSE), main="ACF for differenced series")
p2<- autoplot(pacf(diff1, plot=FALSE), main="PACF for differenced series")
grid.arrange(p1,p2, nrow=2)

#arima

arima111 <- arima(x, order = c(1,1,1))
p1<- autoplot(acf(arima111, plot=FALSE), main="ACF for differenced series")
p2<- autoplot(pacf(arima111, plot=FALSE), main="PACF for differenced series")
grid.arrange(p1,p2, nrow=2)

auto.arima(x)
auto.arima(data_ozen[,2])

#which model is best? 
model.ar$aic
model.ma$aic
model.arma$aic
checkresiduals(model.arma)

#FORCASTING

#regression forecasting
z1 <- Z.fixed[ ,1]
z2 <- Z.fixed[ ,2]
z3 <- Z.fixed[ ,3]
model <- lm(x~z1+z2+z3)

model$fitted.values
# Predict the next 53 time points
t.predict <- (n+1):(n+53)
Z.predict <- data.frame(t.predict, sin(2*pi*t.predict/52), cos(2*pi*t.predict/52))
model.predict <- predict(model, newdata=data.frame(z1 = Z.predict[ , 1],
                                                   z2=Z.predict[ ,2], z3=Z.predict[ ,3]), se.fit = TRUE, interval="prediction")

#fitted values
predictions.harmonic<- model.predict$fit[,1]

# Bootstrap Prediction intervals
nboot <- 1000
bootfit <- matrix(NA,nrow=53,ncol=1000)
for (i in 1:nboot){
  bootfit[,i] <- sample(residuals(model),size=53)
  bootfit[,i] <- predict(model, 
                         newdata=data.frame(z1 = Z.predict[ , 1],z2=Z.predict[ ,2],
                                            z3=Z.predict[,3]),se.fit=FALSE)+bootfit[,i]
}
PI.lwr <- apply(bootfit,1, function(x) quantile(sort(x),0.025))
PI.upr <- apply(bootfit,1, function(x) quantile(sort(x),0.975))


# Plot the predictions 
plot(1:209,c(model$fitted.values, model.predict$fit[ ,1]), type="l", 
     xlab="Time Point", ylab="Ozone concentration", ylim=c(1,110), 
     main="Predictions for 2017", col="#fc4e2a")
points(1:156, data_ozen[,2], pch=19, col=alpha("#b10026",0.7))
lines(157:209, model.predict$fit[ ,2], lty=2, col="#fd8d3c",lwd=2)
lines(157:209, model.predict$fit[ ,3], lty=2, col="#fd8d3c",lwd=2)

lines(157:209, PI.lwr, lty=1, col=alpha("#3c93fd",0.7))
lines(157:209, PI.upr, lty=1, col=alpha("#3c93fd",0.7))
lines(1:156, data_ozen[,2], col=alpha("#b10026",0.7))
points(157:209, model.predict$fit[,1], pch=19, col="#fd8d3c")


#forecasting with arima 
model.ar101 <- arima(residual.series, order=c(1,0,1), xreg=data_ozen$date, include.mean=FALSE)
predict.arma<-predict(model.ar101, n.ahead = 53,newxreg=data_ozen_valid$date)
predict.lci <- predict.arma$pred - 1.96*predict.arma$se
predict.uci <- predict.arma$pred + 1.96*predict.arma$se

plot(1:209, c(residual.series, predict.arma$pred), type="l", xlab="Time", ylab="Ozone concentration", 
     main="Forecast for an AR(1) series")
lines(157:209, predict.arma$pred, col="#fc4e2a")
lines(157:209, predict.lci, lty=2, col="#fd8d3c")
lines(157:209, predict.uci, lty=2, col="#fd8d3c")
#add to harmonic model
predictions_arma<- predict.arma$pred+ (model.predict$fit[,1])

#plot 
plot(1:209, c(x, predictions_arma), type="l", xlab="Time", ylab="Ozone concentration", 
     main="Forecast for an AR(101) series")
points(157:209, data_ozen_valid$ozone, col='blue')
points(157:209, predictions_arma, col='green')


#plot predictions with confidence intervals.

predict.lci <- predict.arma$pred - 1.96*predict.arma$se
predict.uci <- predict.arma$pred + 1.96*predict.arma$se
predict.lci <- predict.lci+(model.predict$fit[,1])
predict.uci <- predict.uci+(model.predict$fit[,1])


plot(1:209, c(x, predictions_arma), type="l", xlab="Time", ylab="Ozone concentration", 
     main="Forecast for an AR(101) series")
lines(157:209, predict.lci, lty=2, col="#fd8d3c")
lines(157:209, predict.uci, lty=2, col="#fd8d3c")
points(157:209, data_ozen_valid$ozone, col='blue')


#forcast the plain arima model
x %>%
  Arima(order=c(1,0,1)) %>%
  forecast() %>%
  autoplot() +
  ylab("H02 sales (million scripts)") + xlab("Year")


##ar(2) forecast 
model <- arima(residual.series, order=c(0,0,2), xreg=data_ozen$date,include.mean=FALSE)
predict.ar2 <- predict(model, n.ahead = 53, newxreg = data_ozen_valid$date, se.fit = TRUE)
predict.lci <- predict.ar2$pred - 1.96*predict.ar2$se
predict.uci <- predict.ar2$pred + 1.96*predict.ar2$se
predict.ar2$pred

plot(1:209, c(residual.series, predict.ar2$pred), type="l", xlab="Time", ylab="Ozone concentration", 
     main="Forecast for an AR(2) series")
lines(157:209, predict.ar2$pred, col="#fc4e2a")
lines(157:209, predict.lci, lty=2, col="#fd8d3c")
lines(157:209, predict.uci, lty=2, col="#fd8d3c")

#add to harmonic model
predictions_ar2<- predict.ar2$pred+ (model.predict$fit[,1])
#plot predictions with actual values
plot(1:209, c(x, predictions_ar2), type="l", xlab="Time", ylab="Ozone concentration", 
     main="Forecast for an AR(2) series")
points(157:209, data_ozen_valid$ozone, col='blue')
points(157:209, predictions_ar2, col='green')

#plot predictions with confidence intervals.

predict.lci <- predict.ar2$pred - 1.96*predict.ar2$se
predict.uci <- predict.ar2$pred + 1.96*predict.ar2$se
predict.lci <- predict.lci+(model.predict$fit[,1])
predict.uci <- predict.uci+(model.predict$fit[,1])


plot(1:209, c(x, predictions_ar2), type="l", xlab="Time", ylab="Ozone concentration", 
     main="Forecast for an AR(2) series")
lines(157:209, predict.lci, lty=2, col="#fd8d3c")
lines(157:209, predict.uci, lty=2, col="#fd8d3c")
points(157:209, data_ozen_valid$ozone, col='blue')


#arima
#Bias
mean(predictions_arma - data_ozen_valid$ozone)
#RMSE
sqrt(mean((predictions_arma-data_ozen_valid$ozone)^2))
#Coverage 
results<- as.numeric(data_ozen_valid$ozone>predict.lci & data_ozen_valid$ozone < predict.uci)
n.simulation<-length(data_ozen_valid$ozone)
100*sum(results) / n.simulation

#ar2
#Bias
mean(predictions_ar2 - data_ozen_valid$ozone)
#RMSE
sqrt(mean((predictions_ar2-data_ozen_valid$ozone)^2))
#Coverage - change predictions intervals for arma and ar1
results<- as.numeric(data_ozen_valid$ozone>predict.lci & data_ozen_valid$ozone < predict.uci)
n.simulation<-length(data_ozen_valid$ozone)
100*sum(results) / n.simulation

#TESTING DATA
#next 36 time points

test_data<-  read.csv('Air_pollution_36.csv')
test_data$date <- as.Date(test_data$Date)
str(data)

test_data<- as.data.frame(cbind(date=test_data$Date, ozone=test_data$Ozone))
test_data$date <- as.Date(test_data$date)
test_data$ozone <- as.numeric(test_data$ozone)


data36<- test_data[1:36,]


#create residual sereis. 
#detrend data using Harmonic model:
x <- data[,8]
n <- length(x)
t <- 1:n
data_harm<- (c())

# linear trend with harmonic seasonal component:
Z.fixed <- cbind(t, sin(2*pi*t/52), cos(2*pi*t/52))
data_harm$trend.fixed <- lm(x~Z.fixed)$fitted.values
data_harm$x.fixed <- x - data_harm$trend.fixed
data_harm<- as.data.frame(data_harm)

#plot harmonic detrended data
ggplot( ) +
  aes(data$Date2, data_harm$x.fixed)+
  geom_line(color = "#d73027", alpha=0.7) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 year") +
  xlab("Date") + ylab("Ozone") +
  ggtitle("Harmonic model for De-trended data")

#check residuals 
model.season <- lm(x~sin(2*pi*t/52) + cos(2*pi*t/52))
residual.series <- x - model.season$fitted.values
summary(model.season)


#forecasting with arima 
model.ar101 <- arima(residual.series, order=c(1,0,1), xreg=data$Date2, include.mean=FALSE)
predict.arma<-predict(model.ar101, n.ahead = 36,newxreg=data36$date)
predict.lci <- predict.arma$pred - 1.96*predict.arma$se
predict.uci <- predict.arma$pred + 1.96*predict.arma$se

plot(1:245, c(residual.series, predict.arma$pred), type="l", xlab="Time", ylab="Ozone concentration", 
     main="Forecast for an AR(1) series")
lines(210:245, predict.arma$pred, col="#fc4e2a")
lines(210:245, predict.lci, lty=2, col="#fd8d3c")
lines(210:245, predict.uci, lty=2, col="#fd8d3c")

z1 <- Z.fixed[ ,1]
z2 <- Z.fixed[ ,2]
z3 <- Z.fixed[ ,3]
model <- lm(x~z1+z2+z3)

model$fitted.values
# Predict the next 36 time points
t.predict <- (n+1):(n+36)
Z.predict <- data.frame(t.predict, sin(2*pi*t.predict/52), cos(2*pi*t.predict/52))
model.predict <- predict(model, newdata=data.frame(z1 = Z.predict[ , 1],
                                                   z2=Z.predict[ ,2], z3=Z.predict[ ,3]), se.fit = TRUE, interval="prediction")

#fitted values
predictions.harmonic<- model.predict$fit[,1]

#add to harmonic model
predictions_arma<- predict.arma$pred+ (model.predict$fit[,1])

#plot 
plot(1:245, c(x, predictions_arma), type="l", xlab="Time", ylab="Ozone concentration", 
     main="Forecast for an ARMA(101) series")
points(210:245, data36$ozone, col='blue')
points(210:245, predictions_arma, col='green')


#plot predictions with confidence intervals.

predict.lci <- predict.arma$pred - 1.96*predict.arma$se
predict.uci <- predict.arma$pred + 1.96*predict.arma$se
predict.lci <- predict.lci+(model.predict$fit[,1])
predict.uci <- predict.uci+(model.predict$fit[,1])


plot(1:245, c(x, predictions_arma), type="l", xlab="Time", ylab="Ozone concentration", 
     main="Forecast for an ARMA(101) series")
lines(210:245, predict.lci, lty=2, col="#fd8d3c")
lines(210:245, predict.uci, lty=2, col="#fd8d3c")
points(210:245, data36$ozone, col='blue')

#arma
#Bias
mean(predictions_arma - data36$ozone)
#RMSE
sqrt(mean((predictions_arma-data36$ozone)^2))
rmse(data36$ozone, predictions_arma)
#Coverage - change predictions intervals for arma and ar1
results<- as.numeric(data36$ozone>predict.lci & data36$ozone < predict.uci)
n.simulation<-length(data36$ozone)
100*sum(results) / n.simulation
#MAE
library(Metrics)
mae(data36$ozone,predictions_arma)
#MAPE
library(MLmetrics)
MAPE(predictions_arma, data36$ozone)

#compare to baseline RMSE
baseline<- auto.arima(data[,8])
predicts<- predict(baseline,n.ahead = 36)


#Bias
mean((predicts$pred) - data36$ozone)
#RMSE
sqrt(mean((predicts$pred-data36$ozone)^2))
rmse(data36$ozone, predicts$pred)
#MAE
library(Metrics)
mae(data36$ozone,predicts$pred)
#MAPE
library(MLmetrics)
MAPE(predicts$pred, data36$ozone)




#next 120 time points

#create residual sereis. 
#detrend data using Harmonic model:
x <- data[,8]
n <- length(x)
t <- 1:n
data_harm<- (c())

# linear trend with harmonic seasonal component:
Z.fixed <- cbind(t, sin(2*pi*t/52), cos(2*pi*t/52))
data_harm$trend.fixed <- lm(x~Z.fixed)$fitted.values
data_harm$x.fixed <- x - data_harm$trend.fixed
data_harm<- as.data.frame(data_harm)

#plot harmonic detrended data
ggplot( ) +
  aes(data$Date2, data_harm$x.fixed)+
  geom_line(color = "#d73027", alpha=0.7) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 year") +
  xlab("Date") + ylab("Ozone") +
  ggtitle("Harmonic model for De-trended data")

#check residuals 
model.season <- lm(x~sin(2*pi*t/52) + cos(2*pi*t/52))
residual.series <- x - model.season$fitted.values
summary(model.season)


#forecasting with arima 
model.ar101 <- arima(residual.series, order=c(1,0,1), xreg=data$Date2, include.mean=FALSE)
predict.arma<-predict(model.ar101, n.ahead = 120,newxreg=test_data$date)
predict.lci <- predict.arma$pred - 1.96*predict.arma$se
predict.uci <- predict.arma$pred + 1.96*predict.arma$se
predict.lci
plot(1:329, c(residual.series, predict.arma$pred), type="l", xlab="Time", ylab="Ozone concentration", 
     main="Forecast for an AR(1) series")
lines(210:329, predict.arma$pred, col="#fc4e2a")
lines(210:329, predict.lci, lty=2, col="#fd8d3c")
lines(210:329, predict.uci, lty=2, col="#fd8d3c")

z1 <- Z.fixed[ ,1]
z2 <- Z.fixed[ ,2]
z3 <- Z.fixed[ ,3]
model <- lm(x~z1+z2+z3)

model$fitted.values
# Predict the next 120 time points
t.predict <- (n+1):(n+120)
Z.predict <- data.frame(t.predict, sin(2*pi*t.predict/52), cos(2*pi*t.predict/52))
model.predict <- predict(model, newdata=data.frame(z1 = Z.predict[ , 1],
                                                   z2=Z.predict[ ,2], z3=Z.predict[ ,3]), se.fit = TRUE, interval="prediction")

#fitted values
predictions.harmonic<- model.predict$fit[,1]

#add to harmonic model
predictions_arma<- predict.arma$pred+ (model.predict$fit[,1])

#plot 
plot(1:329, c(x, predictions_arma), type="l", xlab="Time", ylab="Ozone concentration", 
     main="Forecast for an ARMA(101) series")
points(210:329, test_data$ozone, col='blue')
points(210:329, predictions_arma, col='green')


#plot predictions with confidence intervals.

predict.lci <- predict.arma$pred - 1.96*predict.arma$se
predict.uci <- predict.arma$pred + 1.96*predict.arma$se
predict.lci <- predict.lci+(model.predict$fit[,1])
predict.uci <- predict.uci+(model.predict$fit[,1])


plot(1:329, c(x, predictions_arma), type="l", xlab="Time", ylab="Ozone concentration", 
     main="Forecast for an ARMA(101) series")
lines(210:329, predict.lci, lty=2, col="#fd8d3c")
lines(210:329, predict.uci, lty=2, col="#fd8d3c")
points(210:329, test_data$ozone, col='blue')

#arma
#Bias
mean(predictions_arma - test_data$ozone)
#RMSE
sqrt(mean((predictions_arma-test_data$ozone)^2))
rmse(test_data$ozone, predictions_arma)
#Coverage - change predictions intervals for arma and ar1
results<- as.numeric(test_data$ozone>predict.lci & test_data$ozone < predict.uci)
n.simulation<-length(test_data$ozone)
100*sum(results) / n.simulation
#MAE
library(Metrics)
mae(test_data$ozone,predictions_arma)
#MAPE
library(MLmetrics)
MAPE(predictions_arma, test_data$ozone)
