gpModel = function(arrivals,
                   duration,
                   nRsi = 2,
                   nReg = 2,
                   nVac = 4,
                   rsiFloor = 1,
                   rsiRate = 3,
                   regFloor = 3, 
                   regRate = 1, 
                   vacFloor = 5, 
                   vacRate = 0.5, 
                   obsMean = 20, 
                   obsSD = 0.5,
                   failP = 0.01, 
                   failFloor = 20, 
                   failRate = 1/10){
  
  # Number of arrivals, taking into account no-shows/cancellations
  n <- length(arrivals) 
  
  # Time distributions
  
  ## Average time to prepare one syringe of vaccine
  rsiTime <- rsiFloor + rexp(n, rsiRate)
  
  ## Time to register (temperature check plus pre-vaccination check list)
  regTime <- regFloor + rexp(n, regRate)
  
  ## Time to vaccinate (consent, doffing and jabbing)
  vacTime <- vacFloor + rexp(n, vacRate)
  
  ## Observational times
  obsTime <- ifelse(rbinom(n, 1, failP), 
                    failFloor + rexp(n, failRate),
                    rnorm(n, obsMean, obsSD))
  
  # Queue process
  
  ## Preparation queue
  rsiTarget1 <- ceiling(n/duration)
  rsiTarget2 <- n - rsiTarget1*(duration-1)
  rsiArrivals <- sort(c(rep(seq(0,60*(duration-2), by = 60), rsiTarget1), rep(60*(duration-1) , rsiTarget2)))  
  rsiQueue <- queue_step(arrivals = rsiArrivals, service = rsiTime, servers = nRsi)
  
  ## Registration queue
  regQueue <- queue_step(arrivals, regTime, nReg)
  
  ## Vaccination queue
  vacQueue <- wait_step(regQueue, rsiQueue$departures) %>% queue_step(vacTime, nVac)
  
  ## Observation queue
  obsQueue <- lag_step(vacQueue$departures, obsTime)
  
  
  # Store the service times
  largeN <- 1000
  dfService <- data.frame(
    rsi = rsiFloor + rexp(largeN, rsiRate),
    reg = regFloor + rexp(largeN, regRate),
    vac = vacFloor + rexp(largeN, vacRate),
    obs = ifelse(rbinom(largeN, 1, failP), 
                 failFloor + rexp(largeN, failRate),
                 rnorm(largeN, obsMean, obsSD))
  ) %>%
  mutate(id = row_number())  %>% 
  tidyr::pivot_longer(cols = c(rsi, reg, vac, obs), names_to = 'station', values_to = 'mins')

  ## Code stations as a factor
  dfService$station <- factor(dfService$station, 
                     levels = c('rsi', 'reg', 'vac', 'obs'), 
                     labels = c('Preparation', 'Registration', 'Vaccination', 'Observation'))
  
  # Store the waiting times
  dfQueue <- data.frame(
    rsi = rsiQueue$departures - rsiArrivals,
    reg = regQueue$departures - arrivals,
    vac = vacQueue$departures - regQueue$departures,
    obs = obsQueue - vacQueue$departures,
    tot = obsQueue - arrivals 
  ) %>%
    mutate(id = row_number())  %>% 
    tidyr::pivot_longer(cols = c(rsi, reg, vac, obs, tot), names_to = 'station', values_to = 'mins')
  
  ## Code stations as a factor
  dfQueue$station <- factor(dfQueue$station, 
                            levels = c('rsi', 'reg', 'vac', 'obs', 'tot'), 
                            labels = c('Preparation', 'Registration', 'Vaccination', 'Observation', 'Total'))
  
  # Store the vaccine queue length, utilisation time and number of servers
  sumRsi <-summary(rsiQueue)
  sumReg <-summary(regQueue)
  sumVac <-summary(vacQueue)  

  dfLength <- data.frame(
    rsi = sumRsi$qlength_mean,
    reg = sumReg$qlength_mean,
    vac = sumVac$qlength_mean
  ) %>%
    mutate(id = row_number())  %>%
    tidyr::pivot_longer(cols = c(rsi, reg, vac), names_to = 'station', values_to = 'qLengths')
  
  ## Code stations as a factor
  dfLength$station <- factor(dfLength$station,
                             levels = c('rsi', 'reg', 'vac'), 
                             labels = c('Preparation', 'Registration', 'Vaccination'))
  
  
  dfUtil <- data.frame(
    rsi = sumRsi$utilization,
    reg = sumReg$utilization,
    vac = sumVac$utilization
  ) %>%
    mutate(id = row_number())  %>%
    tidyr::pivot_longer(cols = c(rsi, reg, vac), names_to = 'station', values_to = 'util')
  
  ## Code stations as a factor
  dfUtil$station <- factor(dfUtil$station,
                           levels = c('rsi', 'reg', 'vac'), 
                           labels = c('Preparation', 'Registration', 'Vaccination'))
  
  
  
  ## Store the number of servers
  dfServers <- data.frame(
    rsi = nRsi,
    reg = nReg,
    vac = nVac
  ) %>%
    mutate(id = row_number())  %>%
    tidyr::pivot_longer(cols = c(rsi, reg, vac), names_to = 'station', values_to = 'nServers')
  
  ## Code stations as a factor
  dfServers$station <- factor(dfServers$station,
                              levels = c('rsi', 'reg', 'vac'), 
                              labels = c('Preparation', 'Registration',  'Vaccination'))
  
  output <- list(serviceTime = dfService, 
                 qTimes = dfQueue, 
                 qLengths = dfLength,
                 util = dfUtil,
                 nServers = dfServers)
  
  return(output)
  
}