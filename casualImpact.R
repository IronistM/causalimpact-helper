# Load dependencies
library(lubridate)
library(CausalImpact)
library(xlsx)

if (!file.exists("./data")) {
dir.create("./data")
}

# Get the data, remove NAs and name columns
gdata<-read.csv("data/gdata.csv")
gdata<-gdata[!is.na(gdata$Date),]
names(gdata)<-c("date","users", "conversions")

# Visualise the data
matplot(gdata, type = "l")

causalFunction<-function(data, event="2015-12-31") {
  event<-as.POSIXct(event)
  data$date<-ymd(data$date)
  pre.period<-c( min(data$date), event )
  post.period<-c( event+1, max(data$date) )

  # Prepping for analysis - can add additional explanatory variables after data$users
  data2<-zoo(cbind(data$conversions, data$users), data$date)
  impact<-CausalImpact(data2, pre.period, post.period)
  summary(impact)
  summary(impact, "report")
  plot(impact)
}
