
# Define server logic  
shinyServer(function(input, output, session) {
    
    # initiate hints on startup
    # observe({
    #     if(input$hintsOn1==1){hintjs(session, options = list("hintButtonLabel"="Ok"))}
    # })

    ###############################################
            ### Mass vaccination ###
    ###############################################
    
    # Queue network for mass vaccination
    
    ## render the single-node network
    output$network1 = renderVisNetwork({
    nNodes1 <- 7
    nodes1 <- data.frame(id = 1:nNodes1,
                        
                        title = c(
                                'Arrival | Temperature check | Check in',
                                'Confirm appointment | Pre-vaccination checklist',
                                'Clinical assessment | Record consent',
                                'Expose upper arm | Administer vaccine',
                                'Wait alotted time',
                                'Delivery logistics not modelled here',
                                'Inspect | Log | Draw doses'
                                ),
                        
                        # add labels on nodes
                        label = c('1. Entrance',
                                  '2. Registration',
                                  '3. Assessment',
                                  '4. Vaccination',
                                  '5. Observation',
                                  'A. Delivery',
                                  'B. Preparation'),
                        
                        # size adding value (minimum station = 10 and others scaled accordingly)
                        size = 10*(c(input$nEnt1,  input$nReg1, input$nAss1, input$nVac1, input$nObs1, input$nRsi1, input$nRsi1)/
                                       min(input$nEnt1,  input$nReg1, input$nAss1, input$nVac1, input$nObs1, input$nRsi1)), 
                        
                        # Hierarchical level
                        level = c(1, 2, 3, 4, 5, 2, 3),
                        
                        # Group
                        group = c(rep('Patients', 5), rep('Doses', 2)),
                        
                        # control shape of nodes
                        shape = c(rep("square", 5), rep("triangle", 2)),
                        
                        font.color = c(rep("grey", nNodes1)),
                        
                        # Don't need physics
                        physics = rep(FALSE, nNodes1))
    
    edges1 <- data.frame(
        from = c(1,2,3,4,6,7),
        to = c(2,3,4,5,7,4)
    )
    

        visNetwork(nodes1, edges1) %>%
            visHierarchicalLayout(direction = "LR", levelSeparation = 150) %>%
            visEdges(arrows = "to")  %>%
            visGroups(groupname = "Patients", color = "#bebada") %>%
            visGroups(groupname = "Doses", color = "#fb8072") %>%
            visLegend(enabled = FALSE) %>%
            visInteraction(hover = TRUE) %>%
            visPhysics(stabilization = FALSE) %>%
            visLegend() %>% 
            visEvents(
                # Assign clicked node to input$node_id
                selectNode = "function(data) {
                    Shiny.onInputChange('node_id1', data.nodes);
                    ;}"
            )
    })
    

    # Set up default values for all parameters
    default <- reactiveValues()

    # Entrance defaults
    default$entFloor1 <- reactive(ifelse(is.null(input$entFloor1), 2, input$entFloor1))
    default$entRate1 <- reactive(ifelse(is.null(input$entRate1), 1, input$entRate1))
    
    # Registration defaults
    default$regFloor1 <- reactive(ifelse(is.null(input$regFloor1), 3, input$regFloor1))
    default$regRate1 <- reactive(ifelse(is.null(input$regRate1), 10/7, input$regRate1))
    
    # Assessment defaults
    default$assFloor1 <- reactive(ifelse(is.null(input$assFloor1), 2, input$assFloor1))
    default$assRate1 <- reactive(ifelse(is.null(input$assRate1), 1, input$assRate1))
    
    # Vaccination defaults
    default$vacFloor1 <- reactive(ifelse(is.null(input$vacFloor1), 3, input$vacFloor1))
    default$vacRate1 <- reactive(ifelse(is.null(input$vacRate1), 1, input$vacRate1))
    
    # Observation defaults
    default$obsMean1 <- reactive(ifelse(is.null(input$obsMean1), 20, input$obsMean1))
    default$obsSD1 <- reactive(ifelse(is.null(input$obsSD1), 0.5, input$obsSD1))
    default$failP1 <- reactive(ifelse(is.null(input$failP1), .02, input$failP1))
    default$failFloor1 <- reactive(ifelse(is.null(input$failFloor1), 20, input$failFloor1))
    default$failRate1 <- reactive(ifelse(is.null(input$failRate1), 10, input$failRate1))
    
    # Preparation defaults
    default$rsiFloor1 <- reactive(ifelse(is.null(input$rsiFloor1), 1, input$rsiFloor1))
    default$rsiRate1 <- reactive(ifelse(is.null(input$rsiRate1), 1/3, input$rsiRate1))
    
    # Transition time defaults
    default$ent2regRate1 <- reactive(ifelse(is.null(input$ent2regRate1), 0.5, input$ent2regRate1))
    default$reg2assRate1 <- reactive(ifelse(is.null(input$reg2assRate1), 0.5, input$reg2assRate1))
    default$ass2vacRate1 <- reactive(ifelse(is.null(input$ass2vacRate1), 0.5, input$ass2vacRate1))
    default$vac2obsRate1 <- reactive(ifelse(is.null(input$vac2obsRate1), 0.5, input$vac2obsRate1))
    
    
    # values$setNode and values$setEdge are placeholders for the clicked node and edge
    values <- reactiveValues(setNode1 = 0)
    
    # They start out as NULL so need to replace that with zero
    observe(values$setNode1 <- ifelse(is.null(input$node_id1), 0, input$node_id1))
    
    # They are reverted to 0 and z respectively every time a dialogue box is closed
    observeEvent(input$modal_done,{
        session$sendCustomMessage("node_id", "node_id")
        values$setNode <- 0
        removeModal()
    })
    
    
    observe(
        ## Nodes
        
        # Node 1. Entrance
        if (values$setNode1==1) { isolate(
            showModal(modalDialog(
                h4('Define the service time for the entrance station'),
                column(width = 6,
                       br(),
                       sliderInput("entFloor1", "Baseline entrance time", default$entFloor1(), min = 0, max = 10, step = .1),
                       sliderInput("entRate1", "Entrance time tail", value = default$entRate1(), min = 0.01, max = 5, step = .1),
                       uiOutput('entTimesText1'),
                ),
                column(width = 6,
                       br(),
                       plotOutput('entTimesPlot1')
                ),
                easyClose = FALSE, footer = actionButton("modal_done", label = "Done")))
        )}
        
        # Node 2. Registration
        else if (values$setNode1==2) { isolate(
            showModal(modalDialog(
                h4('Define the service time for the registration station'),
                column(width = 6,
                       br(),
                       sliderInput("ent2regRate1", "Transition time from previous station", default$ent2regRate1(), min = 0, max = 5, step = .1),
                       sliderInput("regFloor1", "Baseline registration time", default$regFloor1(), min = 0, max = 10, step = .1),
                       sliderInput("regRate1", "Registration time tail", value = default$regRate1(), min = 0.01, max = 5, step = .1),
                       uiOutput('regTimesText1'),
                ),
                column(width = 6,
                       br(),
                       plotOutput('regTimesPlot1')
                ),
                easyClose = FALSE, footer = actionButton("modal_done", label = "Done")))
        )}
        
        # Node 3. Assessment
        else if (values$setNode1==3) { isolate(
            showModal(modalDialog(
                h4('Define the service time for the assessment station'),
                column(width = 6,
                       br(),
                       sliderInput("reg2assRate1", "Transition time from previous station", default$reg2assRate1(), min = 0, max = 5, step = .1),
                       sliderInput("assFloor1", "Baseline assessment time", default$assFloor1(), min = 0, max = 10, step = .1),
                       sliderInput("assRate1", "Assessmemt time tail", value = default$assRate1(), min = 0.01, max = 5, step = .1),
                       uiOutput('assTimesText1'),
                ),
                column(width = 6,
                       br(),
                       plotOutput('assTimesPlot1')
                ),
                easyClose = FALSE, footer = actionButton("modal_done", label = "Done")))
        )}
        
        # Node 4. Vaccination
        else if (values$setNode1==4) { isolate(
            showModal(modalDialog(
                h4('Define the service time for the vaccination station'),
                column(width = 6,
                       br(),
                       sliderInput("ass2vacRate1", "Transition time from previous station", default$ass2vacRate1(), min = 0, max = 5, step = .1),
                       sliderInput("vacFloor1", "Baseline vaccination time", default$vacFloor1(), min = 0, max = 10, step = .1),
                       sliderInput("vacRate1", "Vaccination time tail", value = default$vacRate1(), min = 0.01, max = 5, step = .1),
                       uiOutput('vacTimesText1'),
                ),
                column(width = 6,
                       br(),
                       plotOutput('vacTimesPlot1')
                ),
                easyClose = FALSE, footer = actionButton("modal_done", label = "Done")))
        )}
        
        # Node 5. Observation
        else if (values$setNode1==5) { isolate(
            showModal(modalDialog(
                h4('Define the waiting time for the observation station'),
                column(width = 6,
                       br(),
                       sliderInput("vac2obsRate1", "Transition time from previous station", default$vac2obsRate1(), min = 0, max = 5, step = .1),
                       sliderInput("obsMean1", "Baseline observation time", default$obsMean1(), min = 10, max = 30, step = 1),
                       sliderInput("obsSD1", "Standard deviation of observation", value = default$obsSD1(), min = 0.01, max = 5, step = .1),
                       uiOutput('obsTimesText1'),
                ),
                column(width = 6,
                       br(),
                       plotOutput('obsTimesPlot1')
                ),
                
                hr(),
                column(width = 6,
                       br(),
                       h4('Waiting times for patients who experience an adverse reaction'),
                       sliderInput("failP1", "Baseline observation time", default$failP1(), min = 0, max = .2, step = 0.001),
                       sliderInput("failFloor1", "Baseline waiting time", default$failFloor1(), min = 10, max = 30, step = 1),
                       sliderInput("failRate1", "Waiting time tail", default$failRate1(), min = 10, max = 30, step = 1),
                       uiOutput('failTimesText1'),
                ),
                column(width = 6,
                       br(),
                       plotOutput('failTimesPlot1')
                ),
                
                easyClose = FALSE, footer = actionButton("modal_done", label = "Done")))
        )}
        
        
        # Node 7. Preparation
        else if (values$setNode1==7) { isolate(
            showModal(modalDialog(
                h4('Define the service time for the vaccine preparation station'),
                column(width = 6,
                       br(),
                       sliderInput("rsiFloor1", "Baseline prepation time (one dose)", default$rsiFloor1(), min = 0, max = 5, step = .1),
                       sliderInput("rsiRate1", "Preparation time tail", value = default$rsiRate1(), min = 0.01, max = 5, step = .1),
                       uiOutput('rsiTimesText1'),
                ),
                column(width = 6,
                       br(),
                       plotOutput('rsiTimesPlot1')
                       
                ),
                easyClose = FALSE, footer = actionButton("modal_done", label = "Done")))
        )}
        
    ) # Closes observe
    

    # Plots of individual service times
    output$rsiTimesPlot1 <- renderPlot(plotServiceTime(input$rsiFloor1, 1/input$rsiRate1, "Service time", colorHex1), height = 250, width = 250)
    output$entTimesPlot1 <- renderPlot(plotServiceTime(input$entFloor1, 1/input$entRate1, 'Service time', colorHex1), height = 250, width = 250)
    output$regTimesPlot1 <- renderPlot({
        p1 <- plotServiceTime(0, 1/input$ent2regRate1, 'Transtion time', colorHex1)
        p2 <- plotServiceTime(input$regFloor1, 1/input$regRate1, 'Service time', colorHex1)
        ggpubr::ggarrange(p1, p2, ncol=1)})
    output$assTimesPlot1 <- renderPlot({
        p1 <- plotServiceTime(0, 1/input$reg2assRate1, 'Transtion time', colorHex1)
        p2 <- plotServiceTime(input$assFloor1, 1/input$assRate1, 'Service time', colorHex1)
        ggpubr::ggarrange(p1, p2, ncol=1)})
    output$vacTimesPlot1 <- renderPlot({
        p1 <- plotServiceTime(0, 1/input$ass2vacRate1, 'Transtion time', colorHex1)
        p2 <- plotServiceTime(input$vacFloor1, 1/input$vacRate1, 'Service time', colorHex1)
        ggpubr::ggarrange(p1, p2, ncol=1)})
    output$obsTimesPlot1 <- renderPlot({
        p1 <- plotServiceTime(0, 1/input$vac2obsRate1, 'Transtion time', colorHex1)
        p2 <- plotServiceTime2(input$obsMean1, input$obsSD1, 'Service time', colorHex1)
        ggpubr::ggarrange(p1, p2, ncol=1)})
    output$failTimesPlot1 <- renderPlot(plotServiceTime(input$failFloor1, 1/input$failRate1, 'Service time', colorHex1), height = 250, width = 250)
    
    # Text summary of service times
    output$rsiTimesText1 <- renderText(summariseServiceTime(input$rsiFloor1, 1/input$rsiRate1))
    output$entTimesText1 <- renderText(summariseServiceTime(input$entFloor1, 1/input$entRate1))
    output$regTimesText1 <- renderText(summariseServiceTime(input$regFloor1, 1/input$regRate1))
    output$assTimesText1 <- renderText(summariseServiceTime(input$assFloor1, 1/input$assRate1))
    output$vacTimesText1 <- renderText(summariseServiceTime(input$vacFloor1, 1/input$vacRate1))
    output$obsTimesText1 <- renderText(summariseServiceTime2(input$obsMean1, input$obsSD1))
    output$failTimesText1 <- renderText(summariseServiceTime(input$failFloor1, 1/input$failRate1))
    
    output$test <- renderText({
       
    })
    
    # Overall summary of all service times
    output$serviceTimesPlot1 <- renderPlot({
        
        n <- 1000 # Arbitrary high number

        df <- data.frame(
            rsi = default$rsiFloor1() + rexp(n, 1/default$rsiRate1()),
            ent = default$entFloor1() + rexp(n, 1/default$entRate1()),
            reg = default$regFloor1() + rexp(n, 1/default$regRate1()),
            ass = default$assFloor1() + rexp(n, 1/default$assRate1()),
            vac = default$vacFloor1() + rexp(n, 1/default$vacRate1()),
            obs = ifelse(rbinom(n, 1, default$failP1()),
                         default$failFloor1() + rexp(n, 1/default$failRate1()),
                         rnorm(n, default$obsMean1(), default$obsSD1()))
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
                                             nRsi = as.numeric(input$nRsi1),
                                             nEnt = as.numeric(input$nEnt1),
                                             nReg = as.numeric(input$nReg1),
                                             nAss = as.numeric(input$nAss1),
                                             nVac = as.numeric(input$nVac1),
                                             rsiFloor = default$rsiFloor1(),
                                             rsiRate = 1/default$rsiRate1(),
                                             entFloor = default$entFloor1(),
                                             entRate = 1/default$entRate1(),
                                             regFloor = default$regFloor1(),
                                             regRate = 1/default$regRate1(),
                                             assFloor = default$assFloor1(),
                                             assRate = 1/default$assRate1(),
                                             vacFloor = default$vacFloor1(),
                                             vacRate = 1/default$vacRate1(),
                                             obsMean = default$obsMean1(),
                                             obsSD = default$obsSD1(),
                                             ent2regRate = 1/default$ent2regRate1(),
                                             reg2assRate = 1/default$reg2assRate1(),
                                             ass2vacRate = 1/default$ass2vacRate1(),
                                             vac2obsRate = 1/default$vac2obsRate1(),
                                             failP = default$failP1(),
                                             failFloor = default$failFloor1(),
                                             failRate = 1/default$failRate1())
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
                annotate('rect', xmin = 0.5, xmax = 0.7, ymin = 0.5, ymax = 5.5, fill = colorHex1, alpha = 0.2) + 
                geom_boxplot( ) +
                scale_y_discrete(NULL, limits = rev(levels(utilTimes1$station))) +
                scale_x_continuous("Average staff utilisation factor", labels = seq(0,1,.2), limits = c(0,1), breaks = seq(0,1,.2)) +
                theme(text = element_text(size=14))
        })
        
        
        ## Plot the processing time
        output$processing1 <- renderPlot({
            
            ggplot(queueTimes1, aes(y = station, x = mins)) +
                geom_boxplot() +
                scale_y_discrete(NULL, limits = rev(levels(queueTimes1$station))[1:6]) +
                scale_x_continuous("Processing time (minutes)", limits = c(0, NA), breaks = scales::pretty_breaks()) +
                theme(text = element_text(size=14))
        })
        
        
        # Info boxes
        
        ## Vaccinations per day
        output$infoThroughPut1 <- renderText({
            
            queueTimes1 %>% 
                filter(station == 'Total') %>% 
                group_by(iter) %>% 
                summarise(n = max(id)) %>% 
                summarise(mean = median(n)) %>% 
                as.numeric() %>% 
                prettyNum(digits = 0, big.mark=",")
        })
        
        ## Average process time
        output$infoProcessTime1 <- renderText({
            t <- queueTimes1 %>% filter(station == 'Total') %>% summarise(mean = median(mins)) %>% unlist()
            paste(round(t), 'minutes')
        })


    })
    
    # Set up default info boxes
    
    ## Vaccinations per day
    output$infoThroughPut1 <- renderText({
        prettyNum(infoThroughPut1, big.mark=",")
    })

    ## Average process time
    output$infoProcessTime1 <- renderText({
        paste(round(infoProcessTime1), 'minutes')
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
    
    # Queue network for mass vaccination
    
    # render the single-node network
    output$network2 = renderVisNetwork({
        nNodes2 <- 5
        nodes2 <- data.frame(id = 1:nNodes2,
                             
                             title = c(
                                 'Check in | Pre-vaccination checklist',
                                 'Clinical assessment | Record consent | Administer vaccine',
                                 'Wait alotted time',
                                 'Delivery logistics not modelled here',
                                 'Inspect | Log | Draw doses'
                             ),
                             
                             # add labels on nodes
                             label = c('1. Registration',
                                       '2. Vaccination',
                                       '3. Observation',
                                       'A. Delivery',
                                       'B. Preparation'),
                             
                             # size adding value (minimum station = 10 and others scaled accordingly)
                             size = 10*(c(input$nReg2, input$nVac2, input$nObs2, input$nRsi2, input$nRsi2)/
                                            min(input$nReg2, input$nVac2, input$nObs2, input$nRsi2)),
                             
                             # Hierarchical level
                             level = c(2, 3, 4, 1, 2),
                             
                             # Group
                             group = c(rep('Patients', 3), rep('Doses', 2)),
                             
                             # control shape of nodes
                             shape = c(rep("square", 3), rep("triangle", 2)),
                             
                             font.color = c(rep("grey", nNodes2)),
                             
                             # Don't need physics
                             physics = rep(FALSE, nNodes2))
        
        edges2 <- data.frame(
            from = c(1,2,4,5),
            to = c(2,3,5,2)
        )
        
        
        visNetwork(nodes2, edges2) %>%
            visHierarchicalLayout(direction = "LR", levelSeparation = 150) %>%
            visEdges(arrows = "to")  %>%
            visGroups(groupname = "Patients", color = "#bebada") %>%
            visGroups(groupname = "Doses", color = "#fb8072") %>%
            visLegend(enabled = FALSE) %>%
            visInteraction(hover = TRUE) %>%
            visPhysics(stabilization = FALSE) %>%
            visLegend() %>% 
            visEvents(
                # Assign clicked node to input$node_id
                selectNode = "function(data) {
                Shiny.onInputChange('node_id2', data.nodes);
                ;}"
            )
    })

    

    # Registration defaults
    default$regFloor2 <- reactive(ifelse(is.null(input$regFloor2), 3, input$regFloor2))
    default$regRate2 <- reactive(ifelse(is.null(input$regRate2), 1, input$regRate2))

    # Vaccination defaults
    default$vacFloor2 <- reactive(ifelse(is.null(input$vacFloor2), 5, input$vacFloor2))
    default$vacRate2 <- reactive(ifelse(is.null(input$vacRate2), 2/1, input$vacRate2))
    
    # Observation defaults
    default$obsMean2 <- reactive(ifelse(is.null(input$obsMean2), 20, input$obsMean2))
    default$obsSD2 <- reactive(ifelse(is.null(input$obsSD2), 0.5, input$obsSD2))
    default$failP2 <- reactive(ifelse(is.null(input$failP2), .02, input$failP2))
    default$failFloor2 <- reactive(ifelse(is.null(input$failFloor2), 20, input$failFloor2))
    default$failRate2 <- reactive(ifelse(is.null(input$failRate2), 10, input$failRate2))
    
    # Preparation defaults
    default$rsiFloor2 <- reactive(ifelse(is.null(input$rsiFloor2), 1, input$rsiFloor2))
    default$rsiRate2 <- reactive(ifelse(is.null(input$rsiRate2), 1/3, input$rsiRate2))

    # values$setNode ais placeholders for the clicked node and edge
    values <- reactiveValues(setNode2 = 0)
    
    # They start out as NULL so need to replace that with zero
    observe(values$setNode2 <- ifelse(is.null(input$node_id2), 0, input$node_id2))
    
    # They are reverted to 0 and z respectively every time a dialogue box is closed
    observeEvent(input$modal_done,{
        values$setNode1 <- 0
        values$setNode2 <- 0
        removeModal()
    })
    
    
    observe(
        ## Nodes
        
        # Node 2. Registration
        if (values$setNode2==1) { isolate(
            showModal(modalDialog(
                h4('Define the service time for the registration station'),
                column(width = 6,
                       br(),
                       sliderInput("regFloor2", "Baseline registration time", default$regFloor2(), min = 0, max = 10, step = .1),
                       sliderInput("regRate2", "Registration time tail", value = default$regRate2(), min = 0.01, max = 5, step = .1),
                       uiOutput('regTimesText2'),
                ),
                column(width = 6,
                       br(),
                       plotOutput('regTimesPlot2')
                ),
                easyClose = FALSE, footer = actionButton("modal_done", label = "Done")))
        )}
        
        # Node 2. Vaccination
        else if (values$setNode2==2) { isolate(
            showModal(modalDialog(
                h4('Define the service time for the vaccination station'),
                column(width = 6,
                       br(),
                       sliderInput("vacFloor2", "Baseline vaccination time", default$vacFloor2(), min = 0, max = 10, step = .1),
                       sliderInput("vacRate2", "Vaccination time tail", value = default$vacRate2(), min = 0.01, max = 5, step = .1),
                       uiOutput('vacTimesText2'),
                ),
                column(width = 6,
                       br(),
                       plotOutput('vacTimesPlot2')
                ),
                easyClose = FALSE, footer = actionButton("modal_done", label = "Done")))
        )}
        
        # Node 3. Observation
        else if (values$setNode2==3) { isolate(
            showModal(modalDialog(
                h4('Define the waiting time for the observation station'),
                column(width = 6,
                       br(),
                       sliderInput("obsMean2", "Baseline observation time", default$obsMean2(), min = 10, max = 30, step = 1),
                       sliderInput("obsSD2", "Standard deviation of observation", value = default$obsSD2(), min = 0.01, max = 5, step = .1),
                       uiOutput('obsTimesText2'),
                ),
                column(width = 6,
                       br(),
                       plotOutput('obsTimesPlot2')
                ),
                
                hr(),
                column(width = 6,
                       br(),
                       h4('Waiting times for patients who experience an adverse reaction'),
                       sliderInput("failP2", "Baseline observation time", default$failP2(), min = 0, max = .2, step = 0.001),
                       sliderInput("failFloor2", "Baseline waiting time", default$failFloor2(), min = 10, max = 30, step = 1),
                       sliderInput("failRate2", "Waiting time tail", default$failRate2(), min = 10, max = 30, step = 1),
                       uiOutput('failTimesText2'),
                ),
                column(width = 6,
                       br(),
                       plotOutput('failTimesPlot2')
                ),
                
                easyClose = FALSE, footer = actionButton("modal_done", label = "Done")))
        )}
        
        
        # Node 5. Preparation
        else if (values$setNode2==5) { isolate(
            showModal(modalDialog(
                h4('Define the service time for the vaccine preparation station'),
                column(width = 6,
                       br(),
                       sliderInput("rsiFloor2", "Baseline prepation time (one dose)", default$rsiFloor2(), min = 0, max = 5, step = .1),
                       sliderInput("rsiRate2", "Preparation time tail", value = default$rsiRate2(), min = 0.01, max = 5, step = .1),
                       uiOutput('rsiTimesText2'),
                ),
                column(width = 6,
                       br(),
                       plotOutput('rsiTimesPlot2')
                       
                ),
                easyClose = FALSE, footer = actionButton("modal_done", label = "Done")))
        )}
        
    ) # Closes observe
    

    # Plots of individual service times
    output$rsiTimesPlot2 <- renderPlot(plotServiceTime(input$rsiFloor2, 1/input$rsiRate2, "Service time", colorHex2), height = 250, width = 250)
    output$regTimesPlot2 <- renderPlot(plotServiceTime(input$regFloor2, 1/input$regRate2, 'Service time', colorHex2), height = 250, width = 250)
    output$vacTimesPlot2 <- renderPlot(plotServiceTime(input$vacFloor2, 1/input$vacRate2, 'Service time', colorHex2), height = 250, width = 250)
    output$obsTimesPlot2 <- renderPlot(plotServiceTime2(input$obsMean2, input$obsSD2, 'Service time', colorHex2), height = 250, width = 250)
    output$failTimesPlot2 <- renderPlot(plotServiceTime(input$failFloor2, 1/input$failRate2, 'Service time', colorHex2), height = 250, width = 250)
    
    # Text summary of service times
    output$rsiTimesText2 <- renderText(summariseServiceTime(input$rsiFloor2, 1/input$rsiRate2))
    output$regTimesText2 <- renderText(summariseServiceTime(input$regFloor2, 1/input$regRate2))
    output$assTimesText2 <- renderText(summariseServiceTime(input$assFloor2, 1/input$assRate2))
    output$vacTimesText2 <- renderText(summariseServiceTime(input$vacFloor2, 1/input$vacRate2))
    output$failTimesText2 <- renderText(summariseServiceTime(input$failFloor2, 1/input$failRate2))
    
    # Overall summary of all service times
    output$serviceTimesPlot2 <- renderPlot({
        
        n <- 1000 # Arbitrary high number
        
        df <- data.frame(
            rsi = default$rsiFloor2() + rexp(n, 1/default$rsiRate2()),
            reg = default$regFloor2() + rexp(n, 1/default$regRate2()),
            vac = default$vacFloor2() + rexp(n, 1/default$vacRate2()),
            obs = ifelse(rbinom(n, 1, default$failP2()), 
                         default$failFloor2() + rexp(n, 1/default$failRate2()),
                         rnorm(n, default$obsMean2(), default$obsSD2())) 
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
                                                nRsi = as.numeric(input$nRsi2),
                                                nReg = as.numeric(input$nReg2),
                                                nVac = as.numeric(input$nVac2),
                                                rsiFloor = default$rsiFloor2(),
                                                rsiRate = 1/default$rsiRate2(),
                                                regFloor = default$regFloor2(),
                                                regRate = 1/default$regRate2(),
                                                vacFloor = default$vacFloor2(),
                                                vacRate = 1/default$vacRate2(),
                                                obsMean = default$obsMean2(),
                                                obsSD = default$obsSD2(),
                                                failP = default$failP2(),
                                                failFloor = default$failFloor2(),
                                                failRate = 1/default$failRate2())
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
                annotate('rect', xmin = 0.5, xmax = 0.7, ymin = 0.5, ymax = 3.5, fill = colorHex2, alpha = 0.2) + 
                geom_boxplot( ) +
                scale_y_discrete(NULL, limits = rev(levels(utilTimes2$station))) +
                scale_x_continuous("Average staff utilisation factor", labels = seq(0,1,.2), limits = c(0,1), breaks = seq(0,1,.2)) +
                theme(text = element_text(size=14))
        })
        
        
        ## Plot the processing time
        output$processing2 <- renderPlot({
            
            ggplot(queueTimes2, aes(y = station, x = mins)) +
                geom_boxplot() +
                scale_y_discrete(NULL, limits = rev(levels(queueTimes2$station))[1:5]) +
                scale_x_continuous("Processing time (minutes)", limits = c(0, NA), breaks = scales::pretty_breaks()) +
                theme(text = element_text(size=14))
        })
        
        
        # Info boxes
        
        ## Vaccinations per day
        output$infoThroughPut2 <- renderText({
            
            queueTimes2 %>% 
                filter(station == 'Total') %>% 
                group_by(iter) %>% 
                summarise(n = max(id)) %>% 
                summarise(mean = median(n)) %>% 
                as.numeric() %>% 
                prettyNum(digits = 0, big.mark=",")
        })
        
        ## Average process time
        output$infoProcessTime2 <- renderText({
            t <- queueTimes2 %>% filter(station == 'Total') %>% summarise(mean = median(mins)) %>% unlist()
            paste(round(t), 'minutes')
        })
        
        
    })
    
    # Set up default info boxes
    
    ## Vaccinations per day
    output$infoThroughPut2 <- renderText({
        prettyNum(infoThroughPut2, big.mark=",")
    })
    
    ## Average process time
    output$infoProcessTime2 <- renderText({
        paste(round(infoProcessTime2), 'minutes')
    })
    
    
    output$infoHealthStaff2 <- renderText({
        prettyNum(input$nRsi2 + input$nReg2 + + input$nVac2, big.mark=",")
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
