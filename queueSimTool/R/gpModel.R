gpModel = function(arrivals,
                   duration,
                   nRsi = 2,
                   nReg = 2,
                   nVac = 4,
                   nChairs = 10,
                   rsiDist = 'exp',
                   rsiParams = list(1, 3),
                   regDist = 'exp',
                   regParams = list(3, 1),
                   vacDist = 'exp',
                   vacParams = list(5, 0.5),
                   obsDist = 'nrm',
                   obsParams = list(20, 0.5),
                   failDist = 'exp',
                   failParams = list(20, 0.1),
                   failP = 0.01){
  
  # Number of arrivals, taking into account no-shows/cancellations
  n <- length(arrivals) 
  
  # Time distributions
  
  ## Average time to prepare one syringe of vaccine
  rsiTime <- genTimeDist(n, rsiDist, rsiParams)
  
  ## Time to register (temperature check plus pre-vaccination check list)
  regTime <- genTimeDist(n, regDist, regParams)
  
  ## Time to vaccinate (consent, doffing and jabbing)
  vacTime <- genTimeDist(n, vacDist, vacParams)
  
  ## Observational times
  obsTime <- ifelse(rbinom(n, 1, failP), 
                    genTimeDist(n, failDist, failParams),
                    genTimeDist(n, obsDist, obsParams))
  
  # Queue process
  
  ## Preparation queue
  rsiTarget1 <- ceiling(n/duration)
  rsiTarget2 <- n - rsiTarget1*(duration-1)
  rsiArrivals <- sort(c(rep(seq(0,60*(duration-2), by = 60), rsiTarget1), rep(60*(duration-1) , rsiTarget2)))  
  rsiQueue <- queue_step(arrivals = rsiArrivals, service = rsiTime, servers = nRsi)
  
  ## Registration queue
  regQueue <- queue_step(arrivals, regTime, nReg)
  
  ## Vaccination queue
  vacQueue <- wait_step(regQueue$departures, sort(rsiQueue$departures)) %>% queue_step(vacTime, nVac)
  
  ## Observation queue
  obsQueue <- vacQueue$departures %>% queue_step(obsTime, nChairs)
  
  
  # Store the service times
  largeN <- 1000
  dfService <- data.frame(
    rsi = genTimeDist(largeN, rsiDist, rsiParams),
    reg = genTimeDist(largeN, regDist, regParams),
    vac = genTimeDist(largeN, vacDist, vacParams),
    obs = ifelse(rbinom(largeN, 1, failP), 
                 genTimeDist(largeN, failDist, failParams),
                 genTimeDist(largeN, obsDist, obsParams))
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
    obs = obsQueue$departures - vacQueue$departures,
    tot = obsQueue$departures - arrivals 
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
  sumObs <-summary(obsQueue)
  
  dfLength <- data.frame(
    rsi = sumRsi$qlength_mean,
    reg = sumReg$qlength_mean,
    vac = sumVac$qlength_mean,
    obs = sumObs$qlength_mean
  ) %>%
    mutate(id = row_number())  %>%
    tidyr::pivot_longer(cols = c(rsi, reg, vac, obs), names_to = 'station', values_to = 'qLengths')
  
  ## Code stations as a factor
  dfLength$station <- factor(dfLength$station,
                             levels = c('rsi', 'reg', 'vac', 'obs'), 
                             labels = c('Preparation', 'Registration', 'Vaccination', 'Observation'))
  
  
  dfUtil <- data.frame(
    rsi = sumRsi$utilization,
    reg = sumReg$utilization,
    vac = sumVac$utilization,
    obs = sumObs$utilization
  ) %>%
    mutate(id = row_number())  %>%
    tidyr::pivot_longer(cols = c(rsi, reg, vac, obs), names_to = 'station', values_to = 'util')
  
  ## Code stations as a factor
  dfUtil$station <- factor(dfUtil$station,
                           levels = c('rsi', 'reg', 'vac', 'obs'), 
                           labels = c('Preparation', 'Registration', 'Vaccination', 'Observation'))
  
  
  
  ## Store the number of servers
  dfServers <- data.frame(
    rsi = nRsi,
    reg = nReg,
    vac = nVac,
    obs = nChairs
  ) %>%
    mutate(id = row_number())  %>%
    tidyr::pivot_longer(cols = c(rsi, reg, vac, obs), names_to = 'station', values_to = 'nServers')
  
  ## Code stations as a factor
  dfServers$station <- factor(dfServers$station,
                              levels = c('rsi', 'reg', 'vac', 'obs'), 
                              labels = c('Preparation', 'Registration', 'Vaccination', 'Observation'))
  
  output <- list(serviceTime = dfService, 
                 qTimes = dfQueue, 
                 qLengths = dfLength,
                 util = dfUtil,
                 nServers = dfServers)
  
  return(output)
  
}