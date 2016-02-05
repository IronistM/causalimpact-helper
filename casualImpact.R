# https://google.github.io/CausalImpact/CausalImpact.html

impact<-NULL

causalFunction<-function(data, event="2015-01-01") {
  data$date<-as.Date(data$date)
  prev.period<-c(min(data$date), event)
  post.period<-c(event+1, max(data$date))
  data<-as.matrix(data)
  impact <<- CausalImpact(data, pre.period, post.period)
  summary(impact)
  summary(impact, "report")
  plot(impact)
}



# Needed to function
	# 2 cols: date, response variable, with correct classes. 
	# Definition of pre and post period

# Create data

# Set date class 

# Convert to Matrix

# Define periods
pre.period <- as.Date(c("2014-01-01", "2014-03-11"))
post.period <- as.Date(c("2014-03-12", "2014-04-10"))

# Visualise the data
matplot(data, type = "l")

# Run analysis
impact <- CausalImpact(data, pre.period, post.period)

plot(impact)
# Plot 1 = the data and counterfactual prediction for post-treatment period
# Plot 2 = difference between observed data and counterfactual predictions. 
  # This is the pointwise causal effect, as estimated by the model.
# Plot 3 = Adds up the pointwise contributions from the second panel, plotting the cumulative effect of the intervention.

# Output the summary
summary(impact)

# More in-depth
summary(impact, "report")

# Legacy code
	set.seed(1)
	x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 100)
	y <- 1.2 * x1 + rnorm(100)
	y[71:100] <- y[71:100] + 10
	data <- cbind(y, x1)

	# Visualise data
	matplot(data, type = "l")

	pre.period <- c(1, 70)
	post.period <- c(71, 100)

	impact <- CausalImpact(data, pre.period, post.period)

	plot(impact)
	# Plot 1 = the data and counterfactual prediction for post-treatment period
	# Plot 2 = difference between observed data and counterfactual predictions. 
	  # This is the pointwise causal effect, as estimated by the model.
	# Plot 3 = Adds up the pointwise contributions from the second panel, plotting the cumulative effect of the intervention.

	# Using date and times
	time.points <- seq.Date(as.Date("2014-01-01"), by = 1, length.out = 100)
	data <- zoo(cbind(y, x1), time.points)
	head(data)

	pre.period <- as.Date(c("2014-01-01", "2014-03-11"))
	post.period <- as.Date(c("2014-03-12", "2014-04-10"))

	impact <- CausalImpact(data, pre.period, post.period)
	plot(impact)

	# Printing a summary table
	summary(impact)
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

