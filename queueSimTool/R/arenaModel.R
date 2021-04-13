arenaModel = function(arrivals,
                      duration,
                      nRsi = 4,
                      nEnt = 8,
                      nReg = 12,
                      nAss = 8,
                      nVac = 10,
                      rsiFloor = 1,
                      rsiRate = 3,
                      entFloor = 2,
                      entRate = 1,
                      regFloor = 3,
                      regRate = 0.7,
                      assFloor = 2,
                      assRate = 1,
                      vacFloor = 3,
                      vacRate = 1,
                      obsMean = 20, 
                      obsSD = 0.5,
                      ent2regRate = 2,
                      reg2assRate = 2,
                      ass2vacRate = 2,
                      vac2obsRate = 2,
                      failP = 0.02,
                      failFloor = 20,
                      failRate = 1/10){
  
  # Number of arrivals, taking into account no-shows/cancellations
  n <- length(arrivals) 
  
  # Time distributions
  
  ## Average time to prepare one syringe of vaccine
  rsiTime <- rsiFloor + rexp(n, rsiRate)
  
  ## Time entrance station
  entTime <- entFloor + rexp(n, entRate)
  
  ## Time to register (temperature check plus pre-vaccination check list)
  regTime <- regFloor + rexp(n, regRate)
  
  ## Time to vaccinate (consent, doffing and jabbing)
  assTime <- assFloor + rexp(n, assRate)
  
  ## Time to vaccinate (consent, doffing and jabbing)
  vacTime <- vacFloor + rexp(n, vacRate)
  
  ## Observational times
  obsTime <- ifelse(rbinom(n, 1, failP), 
                    failFloor + rexp(n, failRate),
                    rnorm(n, obsMean, obsSD))
  
  ## Times to traverse between the stations
  ent2reg <- rexp(n, ent2regRate)
  reg2ass <- rexp(n, reg2assRate)
  ass2vac <- rexp(n, ass2vacRate)
  vac2obs <- rexp(n, vac2obsRate)
  
  # Queue process

  ## Preparation queue
  rsiTarget1 <- ceiling(n/duration)
  rsiTarget2 <- n - rsiTarget1*(duration-1)
  rsiArrivals <- sort(c(rep(seq(0,60*(duration-2), by = 60), rsiTarget1), rep(60*(duration-1) , rsiTarget2)))  
  rsiQueue <- queue_step(arrivals = rsiArrivals, service = rsiTime, servers = nRsi)

  ## Entrance station
  entQueue <- queue_step(arrivals, entTime, nEnt)

  ## Registration queue
  regQueue <- lag_step(entQueue$departures, ent2reg) %>% queue_step(regTime, nReg) # Lag step is walking time between stations

  ## Assessment queue
  assQueue <- lag_step(regQueue$departures, reg2ass) %>% queue_step(assTime, nAss) # Lag step is walking time between stations

  ## Vaccination queue
  vacQueue <- lag_step(assQueue$departures, ass2vac) %>% wait_step(sort(rsiQueue$departures)) %>% queue_step(vacTime, nVac) # Lag step is walking time between stations

  ## Observation queue
  obsQueue <- lag_step(vacQueue$departures, vac2obs) %>% lag_step(obsTime) # Lag step is walking time between stations


  # Store the service times
  largeN <- 1000
  dfService <- data.frame(
    rsi = rsiFloor + rexp(largeN, rsiRate),
    ent = entFloor + rexp(largeN, entRate),
    reg = regFloor + rexp(largeN, regRate),
    ass = assFloor + rexp(largeN, assRate),
    vac = vacFloor + rexp(largeN, vacRate),
    obs = ifelse(rbinom(largeN, 1, failP),
                 failFloor + rexp(largeN, failRate),
                 rnorm(n, obsMean, obsSD))
  ) %>%
    mutate(id = row_number())  %>%
    tidyr::pivot_longer(cols = c(rsi, ent, reg, ass, vac, obs), names_to = 'station', values_to = 'mins')

  ## Code stations as a factor
  dfService$station <- factor(dfService$station,
                              levels = c('rsi', 'ent', 'reg', 'ass', 'vac', 'obs'),
                              labels = c('Preparation', 'Entrance', 'Registration', 'Assessment', 'Vaccination', 'Observation'))

  # Store the waiting times
  dfQueue <- data.frame(
    rsi = rsiQueue$departures - rsiArrivals,
    ent = entQueue$departures - arrivals,
    reg = regQueue$departures - entQueue$departures,
    ass = assQueue$departures - regQueue$departures,
    vac = vacQueue$departures - assQueue$departures,
    obs = obsQueue - vacQueue$departures,
    tot = obsQueue - arrivals
  ) %>%
    mutate(id = row_number())  %>%
    tidyr::pivot_longer(cols = c(rsi, ent, reg, ass, vac, obs, tot), names_to = 'station', values_to = 'mins')

  ## Code stations as a factor
  dfQueue$station <- factor(dfQueue$station,
                            levels = c('rsi', 'ent', 'reg', 'ass', 'vac', 'obs', 'tot'),
                            labels = c('Preparation', 'Entrance', 'Registration', 'Assessment', 'Vaccination', 'Observation', 'Total'))

  # Store the vaccine queue length and utilisation time
  sumRsi <- summary(rsiQueue)
  sumEnt <- summary(entQueue)
  sumReg <- summary(regQueue)
  sumAss <- summary(assQueue)
  sumVac <- summary(vacQueue)

  dfLength <- data.frame(
    rsi = sumRsi$qlength_mean,
    ent = sumEnt$qlength_mean,
    reg = sumReg$qlength_mean,
    ass = sumAss$qlength_mean,
    vac = sumVac$qlength_mean
  ) %>%
    mutate(id = row_number())  %>%
    tidyr::pivot_longer(cols = c(rsi, ent, reg, ass, vac), names_to = 'station', values_to = 'qLengths')

  ## Code stations as a factor
  dfLength$station <- factor(dfLength$station,
                             levels = c('rsi', 'ent', 'reg', 'ass', 'vac'),
                             labels = c('Preparation', 'Entrance', 'Registration', 'Assessment', 'Vaccination'))


  dfUtil <- data.frame(
    rsi = sumRsi$utilization,
    ent = sumEnt$utilization,
    reg = sumReg$utilization,
    ass = sumAss$utilization,
    vac = sumVac$utilization
  ) %>%
    mutate(id = row_number())  %>%
    tidyr::pivot_longer(cols = c(rsi, ent, reg, ass, vac), names_to = 'station', values_to = 'util')

  ## Code stations as a factor
  dfUtil$station <- factor(dfUtil$station,
                           levels = c('rsi', 'ent', 'reg', 'ass', 'vac'),
                           labels = c('Preparation', 'Entrance', 'Registration', 'Assessment', 'Vaccination'))



  ## Store the number of servers
  dfServers <- data.frame(
    rsi = nRsi,
    ent = nEnt,
    reg = nReg,
    ass = nAss,
    vac = nVac
  ) %>%
    mutate(id = row_number())  %>%
    tidyr::pivot_longer(cols = c(rsi, ent, reg, ass, vac), names_to = 'station', values_to = 'nServers')

  ## Code stations as a factor
  dfServers$station <- factor(dfServers$station,
                              levels = c('rsi', 'ent', 'reg', 'ass', 'vac'),
                              labels = c('Preparation', 'Entrance', 'Registration', 'Assessment', 'Vaccination'))
  
  output <- list(serviceTime = dfService, 
                 qTimes = dfQueue, 
                 qLengths = dfLength,
                 util = dfUtil,
                 nServers = dfServers)
  
  return(output)
  
}