# =============================================================================
# distr.Velocity(...) generates a bimodal gaussian distribution intended to
# simulate retail product velocity behavior online in terms of customer interest
# behavior. Examples are "Adding to Bag", "Purchasing". The idea is that there 
# is a period of activity early in teh day which can be modelled by a smaller
# gaussian distribution whereas activity tapers off and picks up to maximum
# later in the day.

# distr.Velocity(...) takes in the following parameters:
# 'size'
# 'inv.mean'
# 'time.interval' in minutes

# distr.Velocity(...) returns:
#
# =============================================================================

distr.Velocity <- function(day.size, inv.mean){
  # ASSUMPTION: Inventory is stocked to meet at least 10% shopping vel
  ## Create smaller, larger distributions for day, probability of each, then
  ## put them together
  d1 <- rnorm(day.size, mean=inv.mean*0.05, sd=inv.mean*0.01)
  d2 <- rnorm(day.size, mean=inv.mean*0.1, sd=inv.mean*0.01)
  flag <- rbinom(day.size, size=1, prob=0.3)
  
  day.vel <- d1*flag + d2*(1-flag)
  dens.day.vel <- hist(day.vel, breaks=day.size, plot=F)[[3]]
  dens.day.vel <- round(dens.day.vel*10^8*runif(length(dens.day.vel)))
  ### sort out the values less than lowest by using a uniform dist.
  dens.day.vel[dens.day.vel<=range(dens.day.vel)[1]] <- 
    round(runif(sum(dens.day.vel<=range(dens.day.vel)[1]), 
                min = range(dens.day.vel)[1]*2,
                max = range(dens.day.vel)[2]/2))
  
  return(dens.day.vel) 
  #ATB Velocity distribution for a day (any number of intervals in minutes)
  
}