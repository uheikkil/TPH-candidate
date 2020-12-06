popmean <- function(dat=dat,intervention="IRS", years) {
  
  # UH: define zones
  zones <- unique(dat$zone)
  
  # pick values for each zone and the desired years
  ind1 <- dat$zone==zones[1] & dat$year %in%years # first zone
  ind2 <- dat$zone==zones[2] & dat$year %in%years # second zone
  
  # multiply intervention data with population for weighted mean, sum both zones
  
  popmean <- sum(dat[ ind1 ,intervention]*dat[ind1,"population"][1]
                 + dat[ind2 ,intervention]*dat[ind2,"population"][1] ) /
    # divide by sum of population   
    sum(unique(dat$population))* length(years)
  
  
  return(popmean)   
  
}
