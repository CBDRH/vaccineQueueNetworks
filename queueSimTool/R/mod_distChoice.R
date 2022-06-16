#' mod_distChoice UI Function
#'
#' @description A shiny Module to provide specify a time distribution
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_distChoiceUI <- function(id, selected='exp', params = list()){
  ns <- NS(id)
  
  fluidRow(
    div(style = '',
    box(width = 12, 
    column(width = 7,
      tabsetPanel(id = NS(id, 'tab'), 
                  selected = selected,
                  type = 'pills',
          
                  tabPanel('Exponential', value='exp',
                             br(),
                             helpText('Define parameters for the Exponential distribution'),  
                             numericInput(NS(id, 'expFloor'), 'Baseline time', value = params[[1]], min = 0, max = NA, step = .1),
                             numericInput(NS(id, 'expRate'), 'Exponential rate', value = params[[2]], min = 0.01, max = NA, step = .1)
                          ),
                           
                  tabPanel('Normal', value='nrm',
                           br(),
                           helpText('Define parameters for the Normal distribution'),
                           numericInput(NS(id, 'nrmMean'), 'Average time', value = params[[1]], min = 0, max = NA, step = .1),
                           numericInput(NS(id, 'nrmSD'), 'Standard deviation', value = params[[2]], min = 0.01, max = NA, step = .1)
                  ),
                  
                  tabPanel('Log-normal', value='lgn',
                           br(),
                           helpText('Define parameters for the Log-normal distribution'),
                           numericInput(NS(id, 'lgnMean'), 'Average time', value = params[[1]], min = 0, max = NA, step = .1),
                           numericInput(NS(id, 'lgnSD'), 'Standard deviation rate', value = params[[2]], min = 0.01, max = NA, step = .1)
                  ),
                  
                  tabPanel('Gamma', value='gam',
                           br(),
                           helpText('Define parameters for the Gamma distribution'),
                           numericInput(NS(id, 'gamFloor'), 'Baseline time', value = params[[1]], min = 0, max = NA, step = .1),
                           numericInput(NS(id, 'gamShape'), 'Shape', value = params[[2]], min = 0, max = NA, step = .1),
                           numericInput(NS(id, 'gamScale'), 'Scale', value = 1, min = 0.01, max = NA, step = .1)
                  ),
                  
                  tabPanel('Weibull', value='wei',
                           br(),
                           helpText('Define parameters for the Weibull distribution'),
                           numericInput(NS(id, 'weiFloor'), 'Baseline time', value = params[[1]], min = 0, max = 10, step = .1),
                           numericInput(NS(id, 'weiShape'), 'Shape', value = params[[2]], min = 0, max = 10, step = .1),
                           numericInput(NS(id, 'weiScale'), 'Scale', value = 1, min = 0.01, max = 5, step = .1)
                  )
            ), # Close tabsetPanel
    ), # Close left-hand column 
          
    
    column(width = 5,
         h3('Summary of chosen distribution'),
         plotOutput(NS(id, 'hist'), width = '200px', height = '200px'),
         br(),
         htmlOutput(NS(id, 'text'))
    ) # Close right-hand column
      
      ) # Closes box
    )
           
    
  )
}

#' mod_expParams Server Function
#'
#' @noRd 
mod_distChoiceServer <- function(id, title = NULL, color = '#5489C5'){
  moduleServer(id, function(input, output, session){
    
    n <- 1000
    
    data <- reactive({
      if(input$tab=='exp'){input$expFloor + rexp(1000, input$expRate)}
      else if(input$tab=='nrm'){rnorm(1000, input$nrmMean, input$nrmSD)}
      else if(input$tab=='lgn'){exp(rnorm(1000, input$lgnMean, input$lgnSD))}
      else if(input$tab=='gam'){input$gamFloor + rgamma(1000, shape = input$gamShape, scale = input$gamScale)}
      else if(input$tab=='wei'){input$weiFloor + rweibull(1000, shape = input$weiShape, scale = input$weiScale)}
    })
    
    output$hist <- renderPlot({
      plotServiceTime3(data(), title = title, color = color)
    })
    
    output$text <- renderUI({
      summariseServiceTime3(data())
    })
    
    # Store the chosen parameter distribution and corresponding parameters
    list(
      dist = reactive(input$tab),
      params = reactive({
        if(input$tab=='exp'){list(floor = input$expFloor, rate = input$expRate)}
        else if(input$tab=='nrm'){list(mean = input$nrmMean, sd =  input$nrmSD)}
        else if(input$tab=='lgn'){list(mean = input$lgnMean, sd = input$lgnSD)}
        else if(input$tab=='gam'){list(floor = input$gamFloor, shape = input$gamShape, scale = input$gamScale)}
        else if(input$tab=='wei'){list(floor = input$weiFloor, shape = input$weiShape, scale = input$weiScale)}
      })
    )

  })
}

#' mod_expParams test Function
#'
#' @noRd 
test_distChoice <- function() {
  ui <- fluidPage(
    mod_distChoiceUI('mod1', selected = 'exp', params = list(2, 2, 1))
  )
  server <- function(input, output, session) {
    distMod1 <- mod_distChoiceServer('mod1')
    
  }
  
  shinyApp(ui, server)  
}

## To be copied in the UI
# mod_distChoiceUI('mod1')

## To be copied in the server
# mod_distChoiceServer('mod1')

# Test
test_distChoice()

