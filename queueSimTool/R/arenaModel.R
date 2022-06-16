arenaModel = function(arrivals,
                      duration,
                      nRsi = 4,
                      nEnt = 8,
                      nReg = 12,
                      nAss = 8,
                      nVac = 10,
                      nChairs = 50,
                      rsiDist = 'exp',
                      rsiParams = list(1, 3),
                      entDist = 'exp',
                      entParams = list(2, 1),
                      regDist = 'exp',
                      regParams = list(3, 0.7),
                      assDist = 'exp',
                      assParams = list(2, 1),
                      vacDist = 'exp',
                      vacParams = list(3, 1),
                      obsDist = 'nrm',
                      obsParams = list(20, 0.5),
                      failDist = 'exp',
                      failParams = list(20, 0.1),
                      ent2regDist = 'exp',
                      ent2regParams = list(0, 2),
                      reg2assDist = 'exp',
                      reg2assParams = list(0, 2),
                      ass2vacDist = 'exp',
                      ass2vacParams = list(0, 2),
                      vac2obsDist = 'exp',
                      vac2obsParams = list(0, 2),
                      failP = 0.02){
  
  # Number of arrivals, taking into account no-shows/cancellations
  n <- length(arrivals) 
  
  # Time distributions
  
  ## Average time to prepare one syringe of vaccine
  rsiTime <- genTimeDist(n, rsiDist, rsiParams)
  
  ## Time entrance station
  entTime <- genTimeDist(n, entDist, entParams)
  
  ## Time to register (temperature check plus pre-vaccination check list)
  regTime <- genTimeDist(n, regDist, regParams)
  
  ## Time to vaccinate (consent, doffing and jabbing)
  assTime <- genTimeDist(n, assDist, assParams)
  
  ## Time to vaccinate (consent, doffing and jabbing)
  vacTime <- genTimeDist(n, vacDist, vacParams)
  
  ## Observational times
  obsTime <- ifelse(rbinom(n, 1, failP), 
                    genTimeDist(n, failDist, failParams),
                    genTimeDist(n, obsDist, obsParams))
  
  ## Times to traverse between the stations
  ent2reg <- genTimeDist(n, ent2regDist, ent2regParams)
  reg2ass <- genTimeDist(n, reg2assDist, reg2assParams)
  ass2vac <- genTimeDist(n, ass2vacDist, ass2vacParams)
  vac2obs <- genTimeDist(n, vac2obsDist, vac2obsParams)
  
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
  obsQueue <- lag_step(vacQueue$departures, vac2obs) %>% queue_step(obsTime, nChairs) # Lag step is walking time between stations


  # Store the service times
  largeN <- 1000
  dfService <- data.frame(
    rsi = genTimeDist(largeN, rsiDist, rsiParams),
    ent = genTimeDist(largeN, entDist, entParams),
    reg = genTimeDist(largeN, regDist, regParams),
    ass = genTimeDist(largeN, assDist, assParams),
    vac = genTimeDist(largeN, vacDist, vacParams),
    obs = ifelse(rbinom(largeN, 1, failP), 
                 genTimeDist(largeN, failDist, failParams),
                 genTimeDist(largeN, obsDist, obsParams))
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
    obs = obsQueue$departures - vacQueue$departures,
    tot = obsQueue$departures - arrivals
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
  sumObs <- summary(obsQueue)

  dfLength <- data.frame(
    rsi = sumRsi$qlength_mean,
    ent = sumEnt$qlength_mean,
    reg = sumReg$qlength_mean,
    ass = sumAss$qlength_mean,
    vac = sumVac$qlength_mean,
    obs = sumObs$qlength_mean
  ) %>%
    mutate(id = row_number())  %>%
    tidyr::pivot_longer(cols = c(rsi, ent, reg, ass, vac, obs), names_to = 'station', values_to = 'qLengths')

  ## Code stations as a factor
  dfLength$station <- factor(dfLength$station,
                             levels = c('rsi', 'ent', 'reg', 'ass', 'vac', 'obs'),
                             labels = c('Preparation', 'Entrance', 'Registration', 'Assessment', 'Vaccination', 'Observation'))


  dfUtil <- data.frame(
    rsi = sumRsi$utilization,
    ent = sumEnt$utilization,
    reg = sumReg$utilization,
    ass = sumAss$utilization,
    vac = sumVac$utilization,
    obs = sumObs$utilization
  ) %>%
    mutate(id = row_number())  %>%
    tidyr::pivot_longer(cols = c(rsi, ent, reg, ass, vac, obs), names_to = 'station', values_to = 'util')

  ## Code stations as a factor
  dfUtil$station <- factor(dfUtil$station,
                           levels = c('rsi', 'ent', 'reg', 'ass', 'vac', 'obs'),
                           labels = c('Preparation', 'Entrance', 'Registration', 'Assessment', 'Vaccination', 'Observation'))



  ## Store the number of servers
  dfServers <- data.frame(
    rsi = nRsi,
    ent = nEnt,
    reg = nReg,
    ass = nAss,
    vac = nVac,
    obs = nChairs
  ) %>%
    mutate(id = row_number())  %>%
    tidyr::pivot_longer(cols = c(rsi, ent, reg, ass, vac, obs), names_to = 'station', values_to = 'nServers')

  ## Code stations as a factor
  dfServers$station <- factor(dfServers$station,
                              levels = c('rsi', 'ent', 'reg', 'ass', 'vac', 'obs'),
                              labels = c('Preparation', 'Entrance', 'Registration', 'Assessment', 'Vaccination', 'Observation'))
  
  output <- list(serviceTime = dfService, 
                 qTimes = dfQueue, 
                 qLengths = dfLength,
                 util = dfUtil,
                 nServers = dfServers)
  
  return(output)
  
}