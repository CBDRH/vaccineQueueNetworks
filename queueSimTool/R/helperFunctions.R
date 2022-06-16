## Arrivals every 10 minutes for 8 hours
arrivals <- function(n, interval, mean, sd, ns, startMin = 120, stopMin = 590){
  
  #(arrivals run from 120 mins because the reconsitution process starts at time zero, i.e. two hours before first appointment)
  appointment <- sort(rep(seq(startMin, stopMin, by = interval), n))
  
  nApp <- length(appointment)
  
  # Punctuality (mostly early or on time/occasionally late)
  punctuality <- rnorm(nApp, mean, sd) 
  
  # Occasional No shows
  noShow <- rbinom(nApp, 1, ns)
  
  # Net arrival time (must be positive)
  arrival <- sort(pmax(startMin, queuecomputer::lag_step(appointment, punctuality)[!noShow]))
  
  return(arrival)
}

# Plot arrival times
plotArrivalTimes <- function(arrivals, title, start, headstart, color){

  df <- data.frame(time = arrivals*60 + lubridate::parse_date_time("01-03-2021 00:00", "dmY HM")) + (start*60-headstart)*60
  
  ggplot(data = df, aes(y = 1:nrow(df), x = time)) +
    geom_point(color = color) +
    scale_x_datetime('Arrival time', date_breaks = 'hours', date_labels = '%H') +
    scale_y_continuous('Number of arrivals', breaks = scales::pretty_breaks()) +
    labs(title = title)
}


# Plot the service times based on inputs
plotServiceTime <- function(floor, rate, title, color){
    df <- data.frame(mins = floor + rexp(1000, rate))
    ggplot(df, aes(x=mins)) + geom_histogram() + 
      geom_vline(aes(xintercept = mean(mins)), color = color, alpha = 0.6, size = 1.6) + 
      scale_x_continuous('Time (minutes)', limits = c(0, NA)) +
      labs(title = title) +
      theme(axis.title.y=element_blank(), 
            axis.text.y=element_blank(), 
            axis.ticks.y=element_blank())
}


# Plot the service times based on inputs
plotServiceTime2 <- function(mean, sd, title, color){
  df <- data.frame(mins = rnorm(1000, mean, sd))
  ggplot(df, aes(x=mins)) + geom_histogram() + 
    geom_vline(aes(xintercept = mean(mins)), color = color, alpha = 0.6, size = 1.6) + 
    scale_x_continuous('Time (minutes)', limits = c(0, NA)) +
    labs(title = title) +
    theme(axis.title.y=element_blank(), 
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank())
}

# Plot the service times based on a generic input
plotServiceTime3 <- function(time, title, color){
  df <- data.frame(mins = time)
  ggplot(df, aes(x=mins)) + geom_histogram() + 
    geom_vline(aes(xintercept = median(mins)), color = color, alpha = 0.6, size = 1.6) + 
    scale_x_continuous('Time (minutes)', limits = c(0, NA)) +
    labs(title = title) +
    theme(axis.title.y=element_blank(), 
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank())
}

summariseServiceTime <- function(floor, rate){
  df <- data.frame(mins = floor + rexp(1000, rate))
  mean <- prettyNum(mean(df$mins), digits = 2)
  p95 <- prettyNum(quantile(df$mins, .95), digits = 2) 
  shiny::HTML(paste0('The average service time is ', mean, ' minutes.', br(), 
                     '95% of service times are &leq; ', p95, ' minutes.'))
  
}


summariseServiceTime2 <- function(m, sd){
  df <- data.frame(mins = rnorm(1000, m, sd))
  mean <- prettyNum(mean(df$mins), digits = 2)
  p95 <- prettyNum(quantile(df$mins, .95), digits = 2) 
  shiny::HTML(paste0('The average service time is ', mean, ' minutes.', br(), 
                     '95% of service times are &leq; ', p95, ' minutes.'))
}

summariseServiceTime3 <- function(time){
  median <- prettyNum(median(time), digits = 2)
  p95 <- prettyNum(quantile(time, .95), digits = 2) 
  shiny::HTML(paste0('The median service time is ', median, ' minutes.', br(), 
                     '95% of service times are &leq; ', p95, ' minutes.'))
}


# Create a data frame of service times based on GP model output
getServiceTime <- function(modelOutput){
  map_df(modelOutput, ~tibble(id = .$serviceTime$id,
                              station = .$serviceTime$station,
                              mins = .$serviceTime$mins), .id = "iter") 
}

# Create a data frame of queue times based on GP model output
getQueueTime <- function(modelOutput){
  map_df(modelOutput, ~tibble(id = .$qTime$id,
                              station = .$qTime$station,
                              mins = .$qTime$mins), .id = "iter") 
}


# Create a data frame of queue times based on Arena model output
getUtilTime <- function(modelOutput){
  map_df(modelOutput, ~tibble(station = .$qLengths$station,
                              nServers = .$nServers$nServers,
                              qLengths = .$qLengths$qLengths,
                              util = .$util$util), .id = "iter") 
}



genTimeDist <- function(n, dist, params = list){
  
  time <- if(dist == 'exp') {params[[1]] + rexp(n, params[[2]])} 
          else if(dist == 'nrm') {rnorm(n, params[[1]], params[[2]])}
          else if(dist == 'lgn') {exp(rnorm(n, params[[1]], params[[2]]))}
          else if(dist == 'gam') {params[[1]] + rgamma(n, shape = params[[2]], scale = params[[3]])} 
          else if(dist == 'wei') {params[[1]] + rweibull(n, shape = params[[2]], scale = params[[3]])} 
  
  return(time)
}


