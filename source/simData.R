source('~/MCOMPPI/source/distributeVelocity.R')

# =============================================================================
# simData(...) will simulate the minimal data set of 'size' required to realize
# the MCOM Product Popularity Index (PPI). 
# simData(...) takes in the following parameters:
# 'size'
# 'inv.mean'
# 'time.interval' in minutes
# 
# =============================================================================
simData <- function(size, inv.mean, time.interval){
  
  # Initialize a data frame of required size, then add each column. 
  
  # ----------------------------------------------------------------------------
  # INVENTORY AVAILABILITY: "inv.avail"
  ## A Gaussian distribution is assumed, to begin with the passed in inventory
  ## average. It uses 40% of the average as the variation from mean inventory.
  ## This approximates the "Stock Control Model". 
  ## We're not going to model the stock buffer here. Instead, we'll take all 
  ## 0 and - data points to approximate the lower gaussian ranges.
  dat <- data.frame(inv.avail = as.integer(rnorm(size, 
                                                 mean=inv.mean, 
                                                 sd=0.4*inv.mean)))
  ### flip signs for inv.avail < 0
  dat[dat$inv.avail<0,'inv.avail'] <- dat[dat$inv.avail<0,'inv.avail']*(-1)
  ### switch any remaining zeros to minimum acceptable inventory
  dat[dat$inv.avail==0,'inv.avail'] <- 
    dat[dat$inv.avail==0,'inv.avail']+sd(dat$inv.avail)
  ## Now, replicate each row a random number of times to smooth out the inventory
  ## distribution somewhat.
  dat.smooth <- dat[rep(row.names(dat), as.integer(runif(nrow(dat))*10)),]
  # ----------------------------------------------------------------------------
  
  
  
  # ----------------------------------------------------------------------------
  # Seq is an ordered sequence of record insertions which may be interpreted as a 
  # time series.
  dat.smooth$seq <- 1:nrow(dat.smooth)
  dat.smooth <- dat.smooth[,2:1]
  # ----------------------------------------------------------------------------
  
  
  
  # ATB VELOCITY
  # TOTAL ITEMS IN MCOM BAGS (POTENTIAL CONVERSIONS, ATB VELOCITY)/time interval
  # ----------------------------------------------------------------------------
  # We're going to use a Bimodal Gaussian distribution to simulate the velocity
  # of items being added to bag in a day. The first period is a gaussian dist. 
  # that suggests early day passive buying with a peak, whereas the second period
  # suggests increased buying activity and thus item velocity.
  # Since this distribution simulates velocity over a day, we first break down 
  # the data set by intended time interval.
  day.size <- as.integer((60*24)/time.interval) # minute intervals in a day
  
  ##break down the dataset into days
  dat.days <- (nrow(dat.smooth)/day.size)
  
  ## function that returns a vector of bimodally scattered velocity of length
  ## == day.size
  ## repeat that function for entire dataset
  ## TO DO!! GET rid of for. Use SPLIT for this.
  dat.smooth$ATB.velocity <- 0
  lr = 1
  upr=day.size
  for(i in 1:dat.days){
    dat.smooth$ATB.velocity[lr:upr] <- distr.Velocity(day.size, inv.mean)
    lr <- upr+1
    upr <- upr+day.size
  }
  # ----------------------------------------------------------------------------
  
  # SELL VELOCITY 
  # TOTAL ITEMS SOLD (ACTUAL CONVERSIONS) / time interval
  # ----------------------------------------------------------------------------
  # Similar distribution to simulate add to bag behavior.
  dat.smooth$sell.velocity <- 0
  lr = 1
  upr=day.size
  for(i in 1:dat.days){
    dat.smooth$sell.velocity[lr:upr] <- distr.Velocity(day.size, inv.mean)
    lr <- upr+1
    upr <- upr+day.size
  }
  # ----------------------------------------------------------------------------
  
  # ATB Momentum, Sell Momentum, Conversion Momentum
  # ----------------------------------------------------------------------------
  dat.smooth <- dat.smooth %>% mutate(ATB.momentum = inv.avail*ATB.velocity, 
                                      sell.momentum = inv.avail*sell.velocity, 
                                      conv.proportion = sell.momentum/ATB.momentum,
                                      conv.momentum = abs(ATB.momentum-sell.momentum))
  # ----------------------------------------------------------------------------
  dat.smooth <- 
  # PREDICTION AND SIGNIFICANCE!!!!!!!!
  # ----------------------------------------------------------------------------
  # At each record, fit an AR(1) ARIMA model to the lagging a time interval's 
  # worth of lagging conv.momentum values.
  # Use predict on this model to get the PREDICTED MOMENTUM
  # Once done, indicate whether or not the prediction is SIGNIFICANT
  
  # Tinker with ARIMA interval here.
  ## Can we predict based on 30 mins worth of data? (assuming interval's less
  ## than an hour)
  dat.smooth[dat.smooth$conv.momentum==0,'conv.momentum'] <- sample(dat.smooth$inv.avail, size=length(dat.smooth[dat.smooth$conv.momentum==0,'conv.momentum']))
  dat.smooth$pred.conv.momentum <- 0
  dat.smooth$isSignif <- 0
  time.size <- floor(30/time.interval)
  lwr = 1
  for(i in 1:nrow(dat.smooth)){
      upr <- lwr+time.size
      fit <- lm(conv.momentum~seq, data = dat.smooth[lwr:upr, ])
      if(summary(fit)$coef[2,4] <= 0.1 & !is.nan(summary(fit)$coef[2,4])){
        # Mark as significant if momentum significantly related to time interval
        # Used less than 90% confidence interval... TWEAK THIS!
        dat.smooth$isSignif[upr+1] <- 1
      }
      #dat.smooth$pred.conv.momentum <- predict(fit, newdata=dat.smooth[upr+1,])
      dat.smooth$pred.conv.momentum[upr+1] <- 
        coef(fit)[[1]]+coef(fit)[[2]]*dat.smooth$seq[upr]
      lwr <- upr
  }
  
}