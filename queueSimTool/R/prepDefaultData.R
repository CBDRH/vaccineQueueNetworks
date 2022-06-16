# Only run this once, change FALSE to TRUE below (Need to have arenaModel, gpModel, helperFunctions and colorsin global env)
run_it <- FALSE

if (run_it) { 

  # Source relevant functions
  # source(here::here('queueSimTool/global.R'))
  source(here::here('queueSimTool/R/arenaModel.R'))
  source(here::here('queueSimTool/R/gpModel.R'))
  source(here::here('queueSimTool/R/helperFunctions.R'))
  
  # check if www directory exists, create it if not
  dir.create(file.path( here::here('queueSimTool/'), 'www'), showWarnings = FALSE)
  
  # The default mass vaccination queue
  
  queueDefault1 <- list()
  for (i in 1:20) {
    queueDefault1[[i]] <- arenaModel(arrivals = arrivals(120, 60, 0, 5, 0.02), 
                                     duration = 8,
                                     nRsi = 4, nEnt = 8, nReg = 12, nAss = 8, 
                                     nVac = 10, nChairs = 50)
  }
  
  # Default queue plots
  queueTimes1 <- getQueueTime(queueDefault1)
  utilTimes1 <- getUtilTime(queueDefault1)
  
  
  # The default GP clinic queue
  
  queueDefault2 <- list()
  for (i in 1:20) {
    queueDefault2[[i]] <- gpModel(arrivals = arrivals(4, 10, -3, 1, 0.02), 
                                  duration = 8,
                                  nRsi = 2, nReg = 2, nVac = 4, nChairs = 10)
  }
  # Default queue plots
  queueTimes2 <- getQueueTime(queueDefault2)
  utilTimes2 <- getUtilTime(queueDefault2)
  
  
  
  # Default info boxes
  
  ## Vaccinations per day
  infoThroughPut1 <- queueTimes1 %>% 
                      filter(station == 'Total') %>% 
                      group_by(iter) %>% 
                      summarise(n = max(id)) %>% 
                      summarise(mean = median(n),
                                lcl = quantile(n, .025),
                                ucl = quantile(n, .975))
  save(infoThroughPut1, 
       file = here::here('queueSimTool/www/infoThroughPut1'))
  
  ## Median processing time
  infoProcessTime1 <- queueTimes1 %>% 
                        filter(station == 'Total') %>%
                        group_by(iter) %>% 
                        summarise(mins = median(mins)) %>% 
                        summarise(mean = median(mins),
                                  lcl = quantile(mins, .025),
                                  ucl = quantile(mins, .975))
  save(infoProcessTime1, 
       file = here::here('queueSimTool/www/infoProcessTime1'))
  
  ## Vaccinations per day
  infoThroughPut2 <- queueTimes2 %>% 
                      filter(station == 'Total') %>% 
                      group_by(iter) %>% 
                      summarise(n = max(id)) %>% 
                      summarise(mean = median(n),
                                lcl = quantile(n, .025),
                                ucl = quantile(n, .975))
  save(infoThroughPut2, 
       file = here::here('queueSimTool/www/infoThroughPut2'))
  
  ## Median processing time
  infoProcessTime2 <- queueTimes2 %>% 
                        filter(station == 'Total') %>% 
                        group_by(iter) %>% 
                        summarise(mins = median(mins)) %>% 
                        summarise(mean = median(mins),
                                  lcl = quantile(mins, .025),
                                  ucl = quantile(mins, .975))
  
  save(infoProcessTime2, 
       file = here::here('queueSimTool/www/infoProcessTime2'))
  

  throughputP1 <- queueTimes1 %>%
                    filter(station=='Total') %>%
                    group_by(iter) %>%
                    summarise(n = max(id)) %>%
                    ggplot(aes(x = 1, y = n)) +
                    geom_boxplot() +
                    scale_y_continuous('Number of vaccinations delivered', 
                                       breaks = scales::pretty_breaks()) +
                    theme(axis.title.x=element_blank(),
                          axis.text.x=element_blank(),
                          axis.ticks.x=element_blank(),
                          text = element_text(size=14))
  
  save(throughputP1, 
       file = here::here('queueSimTool/www/throughputP1'))
  
  utilisationP1 <- utilTimes1 %>%
                    ggplot(aes(y = station, x = util)) +
                    annotate('rect', xmin = 0.5, xmax = 0.7, 
                             ymin = 0.5, ymax = 6.5, 
                             fill = colorHex1, alpha = 0.2) +
                    geom_boxplot( ) +
                    scale_y_discrete(NULL, 
                                     limits = rev(levels(utilTimes1$station))) +
                    scale_x_continuous("Average staff utilisation factor", 
                                       labels = seq(0,1,.2), limits = c(0,1), 
                                       breaks = seq(0,1,.2)) +
                    theme(text = element_text(size=14))
  
  save(utilisationP1, 
       file = here::here('queueSimTool/www/utilisationP1'))
  
  processingP1 <- ggplot(queueTimes1, aes(y = station, x = mins)) +
                    geom_boxplot() +
                    scale_y_discrete(NULL, 
                                     limits = rev(levels(queueTimes1$station)[c(7, 2:6)])) +
                    scale_x_continuous("Processing time (minutes)", 
                                       limits = c(0, NA), 
                                       breaks = scales::pretty_breaks()) +
                    theme(text = element_text(size=14))
  
  save(processingP1, 
       file = here::here('queueSimTool/www/processingP1'))
  
  throughputP2 <- queueTimes2 %>%
    filter(station=='Total') %>%
    group_by(iter) %>%
    summarise(n = max(id)) %>%
    ggplot(aes(x = 1, y = n)) +
    geom_boxplot() +
    scale_y_continuous('Number of vaccinations delivered', 
                       breaks = scales::pretty_breaks()) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          text = element_text(size=14))
  
  save(throughputP2, 
       file = here::here('queueSimTool/www/throughputP2'))
  
  utilisationP2 <- utilTimes2 %>%
    ggplot(aes(y = station, x = util)) +
    annotate('rect', xmin = 0.5, xmax = 0.7, 
             ymin = 0.5, ymax = 4.5, fill = colorHex2, alpha = 0.2) +
    geom_boxplot( ) +
    scale_y_discrete(NULL, 
                     limits = rev(levels(utilTimes2$station))) +
    scale_x_continuous("Average staff utilisation factor", 
                       labels = seq(0,1,.2), limits = c(0,1), 
                       breaks = seq(0,1,.2)) +
    theme(text = element_text(size=14))
  
  save(utilisationP2, 
       file = here::here('queueSimTool/www/utilisationP2'))
  
  processingP2 <- ggplot(queueTimes2, aes(y = station, x = mins)) +
    geom_boxplot() +
    scale_y_discrete(NULL, 
                     limits = rev(levels(queueTimes2$station)[c(5, 2, 3, 4)])) +
    scale_x_continuous("Processing time (minutes)", 
                       limits = c(0, NA), 
                       breaks = scales::pretty_breaks()) +
    theme(text = element_text(size=14))
  
  save(processingP2, 
       file = here::here('queueSimTool/www/processingP2'))

} # end conditional processing
