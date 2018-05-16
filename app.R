library(shiny)

ui <- navbarPage(
  "HPS111 Grade Tracker",
  
  
  tabPanel("Planning",
           # Sidebar with a slider input for number of bins
           sidebarLayout(
             sidebarPanel(
               h4("Target Grade"),
               numericInput("target",
                            "I am aiming for a score of x %",
                            value = 65),
               hr(),
               h4("Knowledge Assessment"),
               numericInput(
                 "quiz1",
                 "Quiz 1 (/9):",
                 min = 0,
                 max = 9,
                 value = 0
               ),
               numericInput(
                 "quiz2",
                 "Quiz 2 (/9):",
                 min = 0,
                 max = 9,
                 value = 0
               ),
               numericInput(
                 "quiz3",
                 "Quiz 3 (/12):",
                 min = 0,
                 max = 12,
                 value = 0
               ),
               numericInput(
                 "exam",
                 "Exam (%):",
                 min = 0,
                 max = 12,
                 value = 0
               ),
               
               
               hr(),
               h4("Skills Assessment"),
               p("For accurate results, enter scores after late penalties"),
               numericInput(
                 "at1",
                 "AT1 (%):",
                 min = 0,
                 max = 100,
                 value = 0
               ),
               numericInput(
                 "at2",
                 "AT2 (%):",
                 min = 0,
                 max = 100,
                 value = 0
               )
               
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               #uiOutput("weights"),
               #hr(),
               textOutput("statement"),
               plotOutput("targetPlot")
             )
           )),
  tabPanel(
    "About",
    fluidPage(
      "This was built by Mathew Ling",
      a("(@lingtax)", href = "https://twitter.com/lingtax"),
      " to help students better understand their study targets.",
      p(),
      "It is powered by ",
      strong("R"),
      " and the ",
      strong("ggplot2"),
      " package, and is built in ",
      strong("Shiny"),
      ".",
      
      p(),
      
      "For bug reports and feature requests, please raise an issue at ",
      a("this github repository", href = "https://github.com/Lingtax/GradeTracker/issues/new")
    )
  )
)


# Define server logic 
server <- function(input, output) {
  
  # If NA treat as 0 
  # Count latest non-0 response as time-location in trimester.
  week <- reactive({ifelse(input$exam>0, 13, 
                           ifelse(input$at2>0, 12, 
                                  ifelse(input$quiz3>0, 11,
                                         ifelse(input$at1>0, 9,
                                                ifelse(input$quiz2>0, 7,
                                                       ifelse(input$quiz1>0, 4,0
                                                       ))))))
  })
  
  denom <- reactive({ifelse(input$exam>0, 100, 
                            ifelse(input$at2>0, 70, 
                                   ifelse(input$quiz3>0, 40,
                                          ifelse(input$at1>0, 32,
                                                 ifelse(input$quiz2>0, 12,
                                                        ifelse(input$quiz1>0, 6,0
                                                        ))))))
    })
  
  weights <-
    reactive({
      list(
        quiz1weight = round(input$quiz1 / 9 * 6, 2),
        quiz2weight = round(input$quiz2 / 9 * 6, 2),
        quiz3weight = round(input$quiz3 / 12 * 8, 2),
        examweight  = round(input$exam / 100 * 30, 2),
        at1weight   = round(input$at1 / 100 * 20, 2),
        at2weight   = round(input$at2 / 100 * 20, 2)
      )
    })
  
  output$statement <- renderText({
    paste(
      "The entered grades amount to ",
      sum(
        weights()$quiz1weight,
        weights()$quiz2weight,
        weights()$quiz3weight,
        weights()$examweight,
        weights()$at1weight,
        weights()$at2weight
      ),
      "% out of the ",
      denom(),
      "% available so far. "
    )
  })
  
  
  output$weights <- renderUI({weights()})
  
  
  
  output$targetPlot <- renderPlot({
    
    # df <- data.frame(Series = factor(c("Current", "Current", "Target", "Target", "Projected", "Projected"), levels = c("Current", "Target", "Projected")),
    #                  Date = c(input$Start_date, Sys.Date(), input$Start_date, input$Prog_target_date, Sys.Date(), prog()$proj_date),
    #                  n = c(0, input$Prog_current_n, 0, input$Prog_target_n, input$Prog_current_n, input$Prog_target_n)
    # )
    # 
    # ggplot(df, aes(Date, n, col=Series, linetype=Series)) + geom_line(cex = 1) +
    #   scale_linetype_manual(values = c("solid", "dotted", "dashed")) + 
    #   scale_colour_manual(values = c("Black","Blue", "Red")) +
    #   geom_hline(yintercept = input$Prog_target_n, linetype="dashed") +
    #   theme_classic() 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

