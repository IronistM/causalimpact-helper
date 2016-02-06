# https://google.github.io/CausalImpact/CausalImpact.html
library(lubridate)
library(CausalImpact)
library(xlsx)
impact<-NULL

setwd("/Users/donalp/Dropbox/3-Work/Training/Software stuff/Coding/R/JLRDX GA/Queries run/mal-causal-impact")

gdata<-read.csv("data/gdata.csv")
gdata<-gdata[!is.na(gdata$Date),]
names(gdata)<-c("date","users", "conversions")

# Visualise the data
matplot(gdata, type = "l")

causalFunction<-function(data, event="2015-12-31") {
  y<-data$conversions
  x1<-data$users
  event<-as.POSIXct(event)
  data$date<-ymd(data$date)
  pre.period<-c(min(data$date), event)
  post.period<-c( event+1, max(data$date) )
  data2 <- zoo(cbind(y, x1), data$date)
  impact <<- CausalImpact(data2, pre.period, post.period)
  summary(impact)
  summary(impact, "report")
  plot(impact)
}


		# Average col = avg across time during post-intervention period.
		# Since the 95% CI below doesn't include 0 in the range, 
		# we can conclude that there was a significant effect.

		# Posterior inference {CausalImpact}

		#                          Average        Cumulative  
		# Actual                   117            3511        
		# Prediction (s.d.)        107 (0.42)     3195 (12.60)
		# 95% CI                   [106, 107]     [3171, 3220]
		                                                    
		# Absolute effect (s.d.)   11 (0.42)      316 (12.60) 
		# 95% CI                   [9.7, 11]      [291.7, 340]
		                                                    
		# Relative effect (s.d.)   9.9% (0.39%)   9.9% (0.39%)
		# 95% CI                   [9.1%, 11%]    [9.1%, 11%] 

		# Posterior tail-area probability p:   0.00111
		# Posterior prob. of a causal effect:  99.88901%

