
# Header
header <- dashboardHeader(title = 'Queue Simulation Tool',
                          tags$li(class="dropdown",
                                  tags$a(href='https://github.com/CBDRH/', icon('github'), "Source Code", target="_blank")),
                          tags$li(class="dropdown",
                                  tags$a(href='https://cbdrh.med.unsw.edu.au/study-with-us',
                                         icon('graduation-cap'), "Study with us", target="_blank"))
                          )


# Sidebar
sidebar <- dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Mass Vaccination Hub", tabName = "massVac", icon = icon("hospital-alt")),
                menuItem("GP Clinic", tabName = "gpClinic", icon = icon("clinic-medical")),
                menuItem("About this tool", tabName = "info", icon = icon("info"))
                ),    textOutput('test'),

    conditionalPanel(condition="input.sidebar == 'massVac'",
                     hr(),
                     h4('Control Panel'),
                     numericInput("nSims1", "Number of simulations", min = 1, max = 100, value = 20),
                     actionButton(inputId = "runSim1", "Run",icon = icon("redo"), width = '80%', style = "color: #fff; background-color: #00c0ef; border-color: #00c0ef")
                     ),
    
    conditionalPanel(condition="input.sidebar == 'gpClinic'",
                     hr(),
                     h4('Control Panel'),
                     numericInput("nSims2", "Number of simulations", min = 1, max = 100, value = 20),
                     actionButton(inputId = "runSim2", "Run",icon = icon("redo"), width = '80%', style = "color: #fff; background-color: #00a65a; border-color: #00a65a")
    )
    
) # closes dashboardSidebar


# Body
body <- dashboardBody(
    
    tabItems(
        
        # Mass vaccination hub
        tabItem(tabName = "massVac", 
                fluidPage(

                    
                    # infoBoxes
                    fluidRow(
                        infoBox(width = 3,
                                "Median number of vaccinations", uiOutput("infoThroughPut1"), icon = icon("syringe"), color = color1
                        ),
                        infoBox(width = 3,
                                "Median process time", uiOutput("infoProcessTime1"), icon = icon("clock"), color = color1
                        ),
                        infoBox(width = 3,
                                "Healthcare staff", uiOutput("infoHealthStaff1"), icon = icon("user-md"), color = color1
                        ),
                        infoBox(width = 3,
                                "Support staff", uiOutput("infoSupportStaff1"), icon = icon("users"), color = color1
                        )
                    ),
                    
                    fluidRow(
                        box(width = 12, title = 'Queue performance', collapsible = TRUE, solidHeader = FALSE, status = status1, 
                            box(width = 4, title = 'Number of vaccinations administered', status = status1, solidHeader = TRUE, plotOutput('throughput1')),
                            box(width = 4, title = 'Processing times', status = status1, solidHeader = TRUE, plotOutput('processing1')),
                            box(width = 4, title = 'Staff Utilisation', status = status1, solidHeader = TRUE, plotOutput('utilisation1'))
                        )    
                    ),
                    
                    fluidRow(
                        box(width = 12, 
                            title = 'Model assumptions', 
                            collapsible = TRUE, 
                            collapsed = FALSE, 
                            solidHeader = FALSE, 
                            status = status1, 

                                tabBox(width = 12,

                                    ## Service times
                                    tabPanel(title = span(icon("clock"),'Service times'),
                                             
                                             fluidRow(
                                                 box(width = 6, 
                                                     visNetworkOutput("network1"), 
                                                     status = status1, 
                                                     solidHeader = TRUE,
                                                     title = 'Mass vaccination queue network (click a node to edit the service times)'
                                                     ),
                                                 box(width = 6, 
                                                     plotOutput('serviceTimesPlot1'), 
                                                     status = status1, 
                                                     solidHeader = TRUE, 
                                                     title = 'Summary of assumed service service times by queue station')
                                             ),
                                             
                                             ),
                                    
                                    ## Appointments and arrivals
                                    tabPanel(title = span(icon("calendar"), 'Appointments and arrivals'), width = 12,
                                             
                                             fluidRow(
                                                 
                                                 box(width = 4, title = 'Clinic schedule', status = status1, solidHeader = TRUE,
                                                     inputPanel(
                                                         numericInput("startTime1", "Start time", value = 8, min = 0, max = 23),
                                                         numericInput("duration1", "Clinic duration (hours)", value = 8, min = 1, max = 16),
                                                         numericInput("interval1", "Appointment block (minutes)", value = 60, min = 1),
                                                         numericInput("arrivals1", "Number of arrivals per appoinment block", value = 120, min = 1),
                                                         numericInput("headStart1", "Headstart for vaccine preparation (minutes)", value = 120, min = 15, step = 15)
                                                     )
                                                     
                                                     ),
                                                 
                                                 box(width = 4, title = 'Arrival times', status = status1, solidHeader = TRUE,
                                                         inputPanel(
                                                             sliderInput("punctualityMean1", "Punctuality mean (minutes)", value = 0, min =-20, max = 20, step = 1),
                                                             sliderInput("punctualitySD1", "Punctuality SD (minutes)", value = 5, min =1, max = 20, step = 1),
                                                             sliderInput("noShows1", "Rate of no-shows", min = 0, max = 0.2, value = 0.02, step = 0.001)
                                                         )
                                                     ),
                                                 
                                                 box(width = 4, title = 'Summary of arrivals', status = status1, solidHeader = TRUE,
                                                     plotOutput("arrivalTimesPlot1")
                                                     )
                                                 
                                             ) # Closes fluidRow
                                             
                                             
                                             ),
                                    
                                    ## Staffing
                                    tabPanel(title = span(icon("users"), 'Staffing'), width = 12,
                                             fluidRow(
                                                box(width=6, title = "Queue staff", status = status1, solidHeader = TRUE,
                                                    inputPanel(
                                                        numericInput("nEnt1", "1. Entrance", value = 8, width = '60%'),
                                                        numericInput("nReg1", "2. Registration", value = 12, width = '60%'),
                                                        numericInput("nAss1", "3. Assessment", value = 8, width = '60%'),
                                                        numericInput("nVac1", "4. Vaccination", value = 10, width = '60%'),
                                                        numericInput("nObs1", "5. Observation", value = 4, width = '60%'),
                                                        numericInput("nRsi1", "B. Preparation", value = 4, width = '60%')
                                                    )
                                             ),
                                                box(width=6, title = 'Support staff', status = status1, solidHeader = TRUE,
                                                    inputPanel(
                                                        numericInput("nAdm1", "Administrative", value = 2, width = '60%'),
                                                        numericInput("nMar1", "Marshalls", value = 2, width = '60%'),
                                                        numericInput("nSec1", "Security", value = 2, width = '60%'),
                                                        numericInput("nCln1", "Cleaning", value = 2, width = '60%'),
                                                        numericInput("nCat1", "Catering", value = 2, width = '60%'),
                                                        numericInput("nOth1", "Other", value = 2, width = '60%')
                                                    )
                                             )
                                             
                                        )
                                    ) 

                                ) # Closes tabBox
                        ) # Closes first box 
                    ), # Closes fluidRow

                        
        ) # Closes fluidPage
                
    ), # closes massVac tabItem
        
        
        
        
        
        
        
        
        
        
#############################################################################
                            ### GP CLINICS ###        
#############################################################################
        
        tabItem(tabName = "gpClinic", 
                fluidPage(
                    
                    # infoBoxes
                    fluidRow(
                        infoBox(width = 3,
                                "Median number of vaccinations", uiOutput("infoThroughPut2"), icon = icon("syringe"), color = color2
                        ),
                        infoBox(width = 3,
                                "Median process time", uiOutput("infoProcessTime2"), icon = icon("clock"), color = color2
                        ),
                        infoBox(width = 3,
                                "Healthcare staff", uiOutput("infoHealthStaff2"), icon = icon("user-md"), color = color2
                        ),
                        infoBox(width = 3,
                                "Support staff", uiOutput("infoSupportStaff2"), icon = icon("users"), color = color2
                        )
                    ),
                    
                    fluidRow(
                        box(width = 12, title = 'Queue performance', collapsible = TRUE, solidHeader = FALSE, status = status2, 
                            box(width = 4, title = 'Number of vaccinations administered', status = status2, solidHeader = TRUE, plotOutput('throughput2')),
                            box(width = 4, title = 'Processing times', status = status2, solidHeader = TRUE, plotOutput('processing2')),
                            box(width = 4, title = 'Staff Utilisation', status = status2, solidHeader = TRUE, plotOutput('utilisation2'))
                        )    
                    ),
                    
                    fluidRow(
                        box(width = 12, 
                            title = 'Model assumptions', 
                            collapsible = TRUE, 
                            collapsed = FALSE, 
                            solidHeader = FALSE, 
                            status = status2, 
                            
                            tabBox(width = 12,
                                   
                                   ## Service times
                                   tabPanel(title = span(icon("clock"),'Service times'),
                                            
                                            fluidRow(
                                                box(width = 6, 
                                                    visNetworkOutput("network2"), 
                                                    status = status2, 
                                                    solidHeader = TRUE,
                                                    title = 'GP Clinic queue network (click a node to edit the service times)'
                                                ),
                                                box(width = 6, 
                                                    plotOutput('serviceTimesPlot2'), 
                                                    status = status2, 
                                                    solidHeader = TRUE, 
                                                    title = 'Summary of assumed service service times by queue station')
                                            ),
                                            
                                   ),
                                   
                                   ## Appointments and arrivals
                                   tabPanel(title = span(icon("calendar"), 'Appointments and arrivals'), width = 12,
                                            
                                            fluidRow(
                                                
                                                box(width = 4, title = 'Clinic schedule', status = status2, solidHeader = TRUE,
                                                    inputPanel(
                                                        numericInput("startTime2", "Start time", value = 8, min = 0, max = 23),
                                                        numericInput("duration2", "Clinic duration (hours)", value = 8, min = 1, max = 16),
                                                        numericInput("interval2", "Appointment block (minutes)", value = 10, min = 1),
                                                        numericInput("arrivals2", "Number of arrivals per appoinment block", value = 4, min = 1),
                                                        numericInput("headStart2", "Headstart for vaccine preparation (minutes)", value = 120, min = 15, step = 15)
                                                    )
                                                    
                                                ),
                                                
                                                box(width = 4, title = 'Arrival times', status = status2, solidHeader = TRUE,
                                                    inputPanel(
                                                        sliderInput("punctualityMean2", "Punctuality mean (minutes)", value = -3, min =-20, max = 20, step = 1),
                                                        sliderInput("punctualitySD2", "Punctuality SD (minutes)", value = 1, min =1, max = 20, step = 1),
                                                        sliderInput("noShows2", "Rate of no-shows", min = 0, max = 0.2, value = 0.02, step = 0.001)
                                                    )
                                                ),
                                                
                                                box(width = 4, title = 'Summary of arrivals', status = status2, solidHeader = TRUE,
                                                    plotOutput("arrivalTimesPlot2")
                                                )
                                                
                                            ) # Closes fluidRow
                                            
                                            
                                   ),
                                   
                                   ## Staffing
                                   tabPanel(title = span(icon("users"), 'Staffing'), width = 12,
                                            fluidRow(
                                                box(width=6, title = "Queue staff", status = status2, solidHeader = TRUE,
                                                    inputPanel(
                                                        numericInput("nReg2", "1. Registration", value = 2, width = '60%'),
                                                        numericInput("nVac2", "2. Vaccination", value = 4, width = '60%'),
                                                        numericInput("nObs2", "3. Observation", value = 1, width = '60%'),
                                                        numericInput("nRsi2", "B. Preparation", value = 2, width = '60%')
                                                    )
                                                ),
                                                box(width=6, title = 'Support staff', status = status2, solidHeader = TRUE,
                                                    inputPanel(
                                                        numericInput("nAdm2", "Administrative", value = 1, width = '60%'),
                                                        numericInput("nMar2", "Marshalls", value = 1, width = '60%'),
                                                        numericInput("nSec2", "Security", value = 0, width = '60%'),
                                                        numericInput("nCln2", "Cleaning", value = 1, width = '60%'),
                                                        numericInput("nCat2", "Catering", value = 0, width = '60%'),
                                                        numericInput("nOth2", "Other", value = 0, width = '60%')
                                                    )
                                                )
                                                
                                            )
                                   ) 
                                   
                            ) # Closes tabBox
                        ) # Closes first box 
                    ) # Closes fluidRow
                    
                    
                ) # Closes fluidPage
                
                
                
                
                
                ) # Closes gpclinic tabItem
        
        ) # Closes tabItems
    
) # closes dashboardBody


# Put it all together
dashboardPage(header, sidebar, body)