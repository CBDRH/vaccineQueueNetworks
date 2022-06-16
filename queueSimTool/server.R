
# Define server logic  
shinyServer(function(input, output, session) {
    
    # initiate hints on startup
    # observe({
    #     if(input$hintsOn1==1){hintjs(session, options = list("hintButtonLabel"="Ok"))}
    # })
    
    # show intro modal
    observeEvent("", {
        showModal(modalDialog(
            includeMarkdown("intro_text.md"),
            size = 'l',
            easyClose = TRUE,
            fade = TRUE,
            footer = NULL
        ))
    })

    ###############################################
            ### Mass vaccination ###
    ###############################################
    
    # Number of arrivals
    # n1 <- reactive({
    #     length(arrivalTimes1())
    # })
    # 

    # Server side for parameter distribution modules
    rsiDist1 <- mod_distChoiceServer('rsi1')
    entDist1 <- mod_distChoiceServer('ent1')
    regDist1 <- mod_distChoiceServer('reg1')
    assDist1 <- mod_distChoiceServer('ass1')
    vacDist1 <- mod_distChoiceServer('vac1')
    obsDist1 <- mod_distChoiceServer('obs1')
    failDist1 <- mod_distChoiceServer('fail1')
    ent2regDist1 <- mod_distChoiceServer('ent2reg1')
    reg2assDist1 <- mod_distChoiceServer('reg2ass1')
    ass2vacDist1 <- mod_distChoiceServer('ass2vac1')
    vac2obsDist1 <- mod_distChoiceServer('vac2obs1')

    
    # Overall summary of all service times
    output$serviceTimesPlot1 <- renderPlot({
        
        n <- 1000 # Arbitrary high number

        df <- data.frame(
            rsi = genTimeDist(n, dist = rsiDist1$dist(), params = rsiDist1$params()),
            ent = genTimeDist(n, dist = entDist1$dist(), params = entDist1$params()),
            reg = genTimeDist(n, dist = regDist1$dist(), params = regDist1$params()),
            ass = genTimeDist(n, dist = assDist1$dist(), params = assDist1$params()),
            vac = genTimeDist(n, dist = vacDist1$dist(), params = vacDist1$params()),
            obs = ifelse(rbinom(n, 1, input$noShows1),
                         genTimeDist(n, dist = failDist1$dist(), params = failDist1$params()),
                         genTimeDist(n, dist = obsDist1$dist(), params = obsDist1$params()))
        ) %>% mutate(id = row_number())  %>%
            tidyr::pivot_longer(cols = c(rsi, ent, reg, ass, vac, obs), names_to = 'station', values_to = 'mins')
        
        ## Code stations as a factor
        df$station <- factor(df$station, levels = c('rsi', 'ent', 'reg', 'ass', 'vac', 'obs'),
                             labels = c('Preparation', 'Entrance', 'Registration', 'Assessment', 'Vaccination', 'Observation'))
        
        dfMeans <- df %>% group_by(station) %>% summarise(mean = median(mins))
        
        df %>%
            group_by(station) %>%
            ggplot(aes(x = mins)) +
                geom_histogram() +
                geom_vline(data = dfMeans, aes(xintercept = mean), color = '#00c0ef', alpha = 0.6, size = 1.6) +
                scale_x_continuous('Time (minutes)', limits = c(0, NA)) +
                facet_wrap(~station, nrow = 2, scales = 'free') +
                theme(axis.title.y=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank(),
                      text = element_text(size=14))
    })


    # Define arrival times
    arrivalTimes1 <- reactive({
                        arrivals(
                            n = input$arrivals1,
                            interval = input$interval1,
                            mean = input$punctualityMean1,
                            sd = input$punctualitySD1,
                            ns = input$noShows1,
                            startMin = input$headStart1,
                            stopMin = input$headStart1 + 60*input$duration1 - input$interval1)
                        })
                              
    
    # Plot arrival times
    output$arrivalTimesPlot1 <- renderPlot({
        plotArrivalTimes(arrivalTimes1(), NULL, input$startTime1, input$headStart1, colorHex1)
    })
    
    ## Plot the throughput
    output$throughput1 <- renderPlot({
        throughputP1
        })
    
    ## Plot the utilisation
    output$utilisation1 <- renderPlot({
        utilisationP1
    })
    
    
    ## Plot the processing time
    output$processing1 <- renderPlot({
        processingP1
    })
    
    
    # Re run mass vaccination simulation
    observeEvent(input$runSim1, {

        queueUpdate1 <- list()
        
        withProgress(message = "Running simulation", {
            
            for(i in 1:input$nSims1){
                
            # Redefine arrivals on each iteration
            arrivalTimes1 <- arrivals(n = input$arrivals1,
                                        interval = input$interval1,
                                        mean = input$punctualityMean1,
                                        sd = input$punctualitySD1,
                                        ns = input$noShows1,
                                        startMin = input$headStart1,
                                        stopMin = input$headStart1 + 60*input$duration1 - input$interval1)
            
            queueUpdate1[[i]] <- arenaModel(arrivals = arrivalTimes1,
                                            duration = input$duration1,
                                             nRsi = as.numeric(input$nRsi1),
                                             nEnt = as.numeric(input$nEnt1),
                                             nReg = as.numeric(input$nReg1),
                                             nAss = as.numeric(input$nAss1),
                                             nVac = as.numeric(input$nVac1),
                                             nChairs = as.numeric(input$nChairs1),
                                             rsiDist = rsiDist1$dist(),
                                             rsiParams = rsiDist1$params(),
                                             entDist = entDist1$dist(),
                                             entParams = entDist1$params(),
                                             regDist = regDist1$dist(),
                                             regParams = regDist1$params(),
                                             assDist = assDist1$dist(),
                                             assParams = assDist1$params(),
                                             vacDist = vacDist1$dist(),
                                             vacParams = vacDist1$params(),
                                             obsDist = obsDist1$dist(),
                                             obsParams = obsDist1$params(),
                                             failDist = failDist1$dist(),
                                             failParams = failDist1$params(),
                                             ent2regDist = ent2regDist1$dist(),
                                             ent2regParams = ent2regDist1$params(),
                                             reg2assDist = reg2assDist1$dist(),
                                             reg2assParams = reg2assDist1$params(),
                                             ass2vacDist = ass2vacDist1$dist(),
                                             ass2vacParams = ass2vacDist1$params(),
                                             vac2obsDist = vac2obsDist1$dist(),
                                             vac2obsParams = vac2obsDist1$params(),
                                             failP = input$noShows1)
            incProgress(amount=1/input$nSims1)
            }
        })
        
        
        queueTimes1 <- getQueueTime(queueUpdate1)
        utilTimes1 <- getUtilTime(queueUpdate1)
        
        ## Plot the throughput
        output$throughput1 <- renderPlot({
            
            queueTimes1 %>%
                filter(station=='Total') %>%
                group_by(iter) %>%
                summarise(n = max(id)) %>%
                ggplot(aes(x = 1, y = n)) +
                geom_boxplot() +
                scale_y_continuous('Number of patients ', breaks = scales::pretty_breaks()) +
                theme(axis.title.x=element_blank(), 
                      axis.text.x=element_blank(), 
                      axis.ticks.x=element_blank(),
                      text = element_text(size=14))
        })
        
        ## Plot the utilisation
        output$utilisation1 <- renderPlot({
            
            utilTimes1 %>%
                ggplot(aes(y = station, x = util)) +
                annotate('rect', xmin = 0.5, xmax = 0.7, ymin = 0.5, ymax = 6.5, fill = colorHex1, alpha = 0.2) + 
                geom_boxplot( ) +
                scale_y_discrete(NULL, limits = rev(levels(utilTimes1$station))) +
                scale_x_continuous("Average staff utilisation factor", labels = seq(0,1,.2), limits = c(0,1), breaks = seq(0,1,.2)) +
                theme(text = element_text(size=14))
        })
        
        
        ## Plot the processing time
        output$processing1 <- renderPlot({
            
            ggplot(queueTimes1, aes(y = station, x = mins)) +
                geom_boxplot() +
                scale_y_discrete(NULL, limits = rev(levels(queueTimes1$station)[c(7, 2:6)])) +
                scale_x_continuous("Processing time (minutes)", limits = c(0, NA), breaks = scales::pretty_breaks()) +
                theme(text = element_text(size=14))
        })
        
        
        # Info boxes
        
        ## Vaccinations per day
        output$infoThroughPut1 <- renderText({
            
            infoThroughPut1 <- queueTimes1 %>% 
                filter(station == 'Total') %>% 
                group_by(iter) %>% 
                summarise(n = max(id)) %>% 
                summarise(mean = median(n),
                          lcl = quantile(n, .025),
                          ucl = quantile(n, .975))
            
            paste0(prettyNum(infoThroughPut1[1], big.mark=",", digits = 0), ' (', 
                   prettyNum(infoThroughPut1[2], big.mark=",", digits = 0), ' - ', 
                   prettyNum(infoThroughPut1[3], big.mark=",", digits = 0), ')')
        })
        
        ## Average process time
        output$infoProcessTime1 <- renderText({
            
            infoProcessTime1 <- queueTimes1 %>% 
                filter(station == 'Total') %>%
                group_by(iter) %>% 
                summarise(mins = median(mins)) %>% 
                summarise(mean = median(mins),
                          lcl = quantile(mins, .025),
                          ucl = quantile(mins, .975))
            
            paste0(prettyNum(infoProcessTime1[1], big.mark=",", digits = 0), ' (', 
                   prettyNum(infoProcessTime1[2], big.mark=",", digits = 0), ' - ', 
                   prettyNum(infoProcessTime1[3], big.mark=",", digits = 0), ')')
        })


    })
    
    # Set up default info boxes
    
    ## Vaccinations per day
    output$infoThroughPut1 <- renderText({
        paste0(prettyNum(infoThroughPut1[1], big.mark=",", digits = 0), ' (', 
               prettyNum(infoThroughPut1[2], big.mark=",", digits = 0), ' - ', 
               prettyNum(infoThroughPut1[3], big.mark=",", digits = 0), ')')
    })

    ## Average process time
    output$infoProcessTime1 <- renderText({
        paste0(prettyNum(infoProcessTime1[1], big.mark=",", digits = 0), ' (', 
               prettyNum(infoProcessTime1[2], big.mark=",", digits = 0), ' - ', 
               prettyNum(infoProcessTime1[3], big.mark=",", digits = 0), ')')
    })


    output$infoHealthStaff1 <- renderText({
        prettyNum(input$nRsi1 + input$nEnt1 + input$nReg1 + input$nAss1 + input$nVac1, big.mark=",")
    })


    output$infoSupportStaff1 <- renderText({
        prettyNum(input$nObs1 + input$nAdm1 + input$nMar1 + input$nSec1 + input$nCln1 + input$nCat1 + input$nOth1, big.mark=",")
    })
    
    
    
    
    
    ###############################################
            ### GP Clinic ###
    ###############################################

    # Server side for parameter distribution modules
    rsiDist2 <- mod_distChoiceServer('rsi2', color = colorHex2)
    regDist2 <- mod_distChoiceServer('reg2', color = colorHex2)
    vacDist2 <- mod_distChoiceServer('vac2', color = colorHex2)
    obsDist2 <- mod_distChoiceServer('obs2', color = colorHex2)
    failDist2 <- mod_distChoiceServer('fail2', color = colorHex2)
    
    # Overall summary of all service times
    output$serviceTimesPlot2 <- renderPlot({
        
        n <- 1000 # Arbitrary high number
        
        df <- data.frame(
            rsi = genTimeDist(n, dist = rsiDist2$dist(), params = rsiDist2$params()),
            reg = genTimeDist(n, dist = regDist2$dist(), params = regDist2$params()),
            vac = genTimeDist(n, dist = vacDist2$dist(), params = vacDist2$params()),
            obs = ifelse(rbinom(n, 1, input$noShows2),
                         genTimeDist(n, dist = failDist2$dist(), params = failDist2$params()),
                         genTimeDist(n, dist = obsDist2$dist(), params = obsDist2$params()))
        ) %>% mutate(id = row_number())  %>% 
            tidyr::pivot_longer(cols = c(rsi, reg, vac, obs), names_to = 'station', values_to = 'mins') 
        
        ## Code stations as a factor
        df$station <- factor(df$station, levels = c('rsi', 'reg', 'vac', 'obs'), 
                             labels = c('Preparation', 'Registration', 'Vaccination', 'Observation'))
        
        dfMeans <- df %>% group_by(station) %>% summarise(mean = median(mins)) 
        
        df %>% 
            group_by(station) %>% 
            ggplot(aes(x = mins)) + 
            geom_histogram() + 
            geom_vline(data = dfMeans, aes(xintercept = mean), color = colorHex2, alpha = 0.6, size = 1.6) + 
            scale_x_continuous('Time (minutes)', limits = c(0, NA)) +
            facet_wrap(~station, nrow = 2, scales = 'free') +
            theme(axis.title.y=element_blank(), 
                  axis.text.y=element_blank(), 
                  axis.ticks.y=element_blank(),
                  text = element_text(size=14))
        
    })
    
    
    # Define arrival times
    arrivalTimes2 <- reactive({
        arrivals(
            n = input$arrivals2,
            interval = input$interval2,
            mean = input$punctualityMean2,
            sd = input$punctualitySD2,
            ns = input$noShows2,
            startMin = input$headStart2,
            stopMin = input$headStart2 + 60*input$duration2 - input$interval2)
    })
    
    
    # Plot arrival times
    output$arrivalTimesPlot2 <- renderPlot({
        plotArrivalTimes(arrivalTimes2(), NULL, input$startTime2, input$headStart2, colorHex2)
    })
    

    ## Plot the throughput
    output$throughput2 <- renderPlot({
         throughputP2
    })
    
    ## Plot the utilisation
    output$utilisation2 <- renderPlot({
        utilisationP2
    })
    
    
    ## Plot the processing time
    output$processing2 <- renderPlot({
        processingP2
    })
    
    
    # Re run mass vaccination simulation
    observeEvent(input$runSim2, {
        
        queueUpdate2 <- list()
        
        withProgress(message = "Running simulation", {
            
            for(i in 1:input$nSims2){
                
                # Redefine arrivals on each iteration
                arrivalTimes2 <- arrivals(n = input$arrivals2,
                                          interval = input$interval2,
                                          mean = input$punctualityMean2,
                                          sd = input$punctualitySD2,
                                          ns = input$noShows2,
                                          startMin = input$headStart2,
                                          stopMin = input$headStart2 + 60*input$duration2 - input$interval2)
                
                queueUpdate2[[i]] <- gpModel(arrivals = arrivalTimes2,
                                             duration = input$duration2,
                                                nRsi = as.numeric(input$nRsi2),
                                                nReg = as.numeric(input$nReg2),
                                                nVac = as.numeric(input$nVac2),
                                                nChairs = as.numeric(input$nChairs2),
                                                rsiDist = rsiDist2$dist(),
                                                rsiParams = rsiDist2$params(),
                                                regDist = regDist2$dist(),
                                                regParams = regDist2$params(),
                                                vacDist = vacDist2$dist(),
                                                vacParams = vacDist2$params(),
                                                obsDist = obsDist2$dist(),
                                                obsParams = obsDist2$params(),
                                                failDist = failDist2$dist(),
                                                failParams = failDist2$params(),
                                                failP = input$noShows2)
                incProgress(amount=1/input$nSims2)
            }
        })
        
        
        queueTimes2 <- getQueueTime(queueUpdate2)
        utilTimes2 <- getUtilTime(queueUpdate2)
        
        ## Plot the throughput
        output$throughput2 <- renderPlot({
            
            
            
            queueTimes2 %>%
                filter(station=='Total') %>%
                group_by(iter) %>%
                summarise(n = max(id)) %>%
                ggplot(aes(x = 1, y = n)) +
                geom_boxplot() +
                scale_y_continuous('Number of patients ', breaks = scales::pretty_breaks()) +
                theme(axis.title.x=element_blank(), 
                      axis.text.x=element_blank(), 
                      axis.ticks.x=element_blank(),
                      text = element_text(size=14))
        })
        
        ## Plot the utilisation
        output$utilisation2 <- renderPlot({
            
            utilTimes2 %>%
                ggplot(aes(y = station, x = util)) +
                annotate('rect', xmin = 0.5, xmax = 0.7, ymin = 0.5, ymax = 4.5, fill = colorHex2, alpha = 0.2) + 
                geom_boxplot( ) +
                scale_y_discrete(NULL, limits = rev(levels(utilTimes2$station))) +
                scale_x_continuous("Average staff utilisation factor", labels = seq(0,1,.2), limits = c(0,1), breaks = seq(0,1,.2)) +
                theme(text = element_text(size=14))
        })
        
        
        ## Plot the processing time
        output$processing2 <- renderPlot({
            
            ggplot(queueTimes2, aes(y = station, x = mins)) +
                geom_boxplot() +
                scale_y_discrete(NULL, limits = rev(levels(queueTimes2$station)[c(5, 2, 3, 4)])) +
                scale_x_continuous("Processing time (minutes)", limits = c(0, NA), breaks = scales::pretty_breaks()) +
                theme(text = element_text(size=14))
        })
        
        
        # Info boxes
        
        ## Vaccinations per day
        output$infoThroughPut2 <- renderText({
            
            infoThroughPut2 <- queueTimes2 %>% 
                filter(station == 'Total') %>% 
                group_by(iter) %>% 
                summarise(n = max(id)) %>% 
                summarise(mean = median(n),
                          lcl = quantile(n, .025),
                          ucl = quantile(n, .975))
            
            paste0(prettyNum(infoThroughPut2[1], big.mark=",", digits = 0), ' (', 
                   prettyNum(infoThroughPut2[2], big.mark=",", digits = 0), ' - ', 
                   prettyNum(infoThroughPut2[3], big.mark=",", digits = 0), ')')
        })
        
        ## Average process time
        output$infoProcessTime2 <- renderText({
            
            infoProcessTime2 <- queueTimes2 %>% 
                filter(station == 'Total') %>%
                group_by(iter) %>% 
                summarise(mins = median(mins)) %>% 
                summarise(mean = median(mins),
                          lcl = quantile(mins, .025),
                          ucl = quantile(mins, .975))
            
            paste0(prettyNum(infoProcessTime2[1], big.mark=",", digits = 0), ' (', 
                   prettyNum(infoProcessTime2[2], big.mark=",", digits = 0), ' - ', 
                   prettyNum(infoProcessTime2[3], big.mark=",", digits = 0), ')')
        })
        
        
    })
    
    # Set up default info boxes
    
    ## Vaccinations per day
    output$infoThroughPut2 <- renderText({
        paste0(prettyNum(infoThroughPut2[1], big.mark=",", digits = 0), ' (', 
               prettyNum(infoThroughPut2[2], big.mark=",", digits = 0), ' - ', 
               prettyNum(infoThroughPut2[3], big.mark=",", digits = 0), ')')
    })
    
    ## Average process time
    output$infoProcessTime2 <- renderText({
        paste0(prettyNum(infoProcessTime2[1], big.mark=",", digits = 0), ' (', 
               prettyNum(infoProcessTime2[2], big.mark=",", digits = 0), ' - ', 
               prettyNum(infoProcessTime2[3], big.mark=",", digits = 0), ')')
    })
    
    
    output$infoHealthStaff2 <- renderText({
        prettyNum(input$nRsi2 + input$nReg2 + input$nVac2, big.mark=",")
    })
    
    
    output$infoSupportStaff2 <- renderText({
        prettyNum(input$nObs2 + input$nAdm2 + input$nMar2 + input$nSec2 + input$nCln2 + input$nCat2 + input$nOth2, big.mark=",")
    })
    
    # start introjs when button is pressed with custom options and events
    observeEvent(input$help1,
                 introjs(session, options = list("nextLabel"="Next",
                                                 "prevLabel"="Previous",
                                                 "skipLabel"="Done"))
    )
})
