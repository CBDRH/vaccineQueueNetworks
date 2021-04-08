

# Header
header <- dashboardHeader(title =  "Vaccination facility simulator",
                          titleWidth = 300,
                          tags$li(class="dropdown",
                                  tags$a(href='https://github.com/CBDRH/', icon('github'), "Source Code", target="_blank")),
                          tags$li(class="dropdown",
                                  tags$a(href='https://cbdrh.med.unsw.edu.au/study-with-us',
                                         icon('graduation-cap'), "Study with us", target="_blank"))
                          )

# Sidebar
sidebar <- dashboardSidebar(
        br(),
        div(style="text-align:center",
            tags$img(src='unsw_logo_reverse.png',height='70',width='165')
        ),
        
        hr(),
        div(style="text-align:center",
            h4(span(icon("project-diagram"),'Vaccination mode'))
            ),
        br(),    
    sidebarMenu(id = "sidebar",
                menuItem("Mass Vaccination Hub", tabName = "massVac", icon = icon("hospital-alt")),
                menuItem("GP Clinic", tabName = "gpClinic", icon = icon("clinic-medical"))
                ), 
    
                br(),
    
    
    conditionalPanel(condition="input.sidebar == 'massVac'",
                     #h4(span(icon("wrench"),'Control panel')),
                     numericInput("nSims1", "Number of simulations", min = 1, max = 100, value = 20),
                     actionButton(inputId = "runSim1", "Run",icon = icon("redo"), width = '80%', style = "color: #fff; background-color: #00c0ef; border-color: #00c0ef")
                     ),
    
    conditionalPanel(condition="input.sidebar == 'gpClinic'",
                    #h4(span(icon("wrench"),'Control panel')),
                     numericInput("nSims2", "Number of simulations", min = 1, max = 100, value = 20),
                     actionButton(inputId = "runSim2", "Run",icon = icon("redo"), width = '80%', style = "color: #fff; background-color: #00a65a; border-color: #00a65a")
    ),
    
    hr(),
    
    sidebarMenu(id = "sidebar", menuItem("About this tool", tabName = "info", icon = icon("info")))  
    

    
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

            HTML("<a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/'><img alt='Creative Commons Licence' style='border-width:0' src='https://i.creativecommons.org/l/by-nc/4.0/88x31.png' /></a><br />This work is licensed under a <a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/'>Creative Commons Attribution-NonCommercial 4.0 International License</a>.")
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
                    ), # Closes fluidRow
                    
                    HTML("<a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/'><img alt='Creative Commons Licence' style='border-width:0' src='https://i.creativecommons.org/l/by-nc/4.0/88x31.png' /></a><br />This work is licensed under a <a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/'>Creative Commons Attribution-NonCommercial 4.0 International License</a>.") 
                ) # Closes fluidPage
                
                ), # Closes gpclinic tabItem
        

                # Mass vaccination hub
                tabItem(tabName = "info", 
                        fluidPage(
                            
                            h4('Contributors'),
                            p('This applet was created by'),
                            tags$ul(
                                tags$li("Mark Hanly"),
                                tags$li("Oisín Fitzgerald"),
                                tags$li(tags$a(href="https://timchurches.github.io/", 
                                               icon="blog", target="_blank",
                                               "Tim Churches"))
                            ),
                            
                            hr(),
                            
                            h4('About queueing network models'),
                            p('The queueing network models implmented in this applet are estimated using the ',
                            tags$code('queuecomputer'),
                            ' package. More information on this package can be found at the links below.'),
                            p(tags$a(href="https://www.jstatsoft.org/article/view/v095i05",
                                     icon("file-pdf"), target="_blank", "Ebert, A., Wu, P., Mengersen, K., & Ruggeri, F. (2020). Computationally Efficient Simulation of Queues: The R Package queuecomputer. Journal of Statistical Software, 95(5), 1 - 29. doi:http://dx.doi.org/10.18637/jss.v095.i05")),
                            
                            hr(),
                            
                            h4('Related preprint scientific paper'),
                            p("Modelling vaccination capacity at mass vaccination hubs and general practice clinics.",
                              br(),
                              "Mark J Hanly, Tim Churches, Oisín Fitzgerald, Ian Caterson, Chandini Raina MacIntyre, Louisa Jorm",
                              br(),
                              "medRxiv 2021.04.07.21255067; doi: ",
                              tags$a(href="https://doi.org/10.1101/2021.04.07.21255067",
                                     icon("file-pdf"), target="_blank", 
                                     "https://doi.org/10.1101/2021.04.07.21255067")),

                            hr(),
                            
                            h4('Source code'),
                            p('The source code for this application can be found at ', tags$a(href="https://github.com/CBDRH/vaccineQueueNetworks", icon("github"), target="_blank", "https://github.com/CBDRH/vaccineQueueNetworks")),
                            p('Bug reports and feature requests can be submitted through the GitHub issues page ',  tags$a(href="https://github.com/CBDRH/vaccineQueueNetworks/issues", icon("github"), target="_blank", "https://github.com/CBDRH/vaccineQueueNetworks/issues")),
                            
                            hr(),
                            
                            h4('Acknowledgements'),
                            p('This research was supported by the generous assistance of Ian Sharp, philanthropic supporter of UNSW research, 
                            and by a research seed grant provided by the', tags$a(href="https://www.thesphere.com.au/work/infectious-disease-immunity-and-inflammation-triple-i", target="_blank",  "Sydney Partnership for Health, Education, Research and Enterprise (SPHERE) 
                            Infectious diseases, Immunity and Inflammation (Triple-I) Clinical Academic Group"), '.'),
                            p('The Wangal, Bedegal and Gadigal people of the Eora Nation are the traditional owners of the land on 
                              which this work was undertaken. We acknowledge and pay our respects to their Elders, both past, present and emerging.'),

                            br(),
                            HTML("<a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/'><img alt='Creative Commons Licence' style='border-width:0' src='https://i.creativecommons.org/l/by-nc/4.0/88x31.png' /></a><br />This work is licensed under a <a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/'>Creative Commons Attribution-NonCommercial 4.0 International License</a>.")     
                        ), # Closes fluidPage
                    
                )

        ) # Closes tabItems
    
) # closes dashboardBody


# Put it all together
dashboardPage(header, 
              sidebar, 
              body)
