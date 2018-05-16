library(shiny)
library(ggplot2)

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
               # numericInput(
               #   "exam",
               #   "Exam (%):",
               #   min = 0,
               #   max = 12,
               #   value = 0
               #),
               
               
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
               textOutput("statement1"),
               textOutput("statement2"),
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
  week <- reactive({#ifelse(input$exam>0, 6, 
                           ifelse(input$at2>0, 6, 
                                  ifelse(input$quiz3>0, 5,
                                         ifelse(input$at1>0, 4,
                                                ifelse(input$quiz2>0, 3,
                                                       ifelse(input$quiz1>0, 2, 1
                                                       )))))#)
  })
  
  denom <- reactive({#ifelse(input$exam>0, 100, 
                            ifelse(input$at2>0, 70, 
                                   ifelse(input$quiz3>0, 40,
                                          ifelse(input$at1>0, 32,
                                                 ifelse(input$quiz2>0, 12,
                                                        ifelse(input$quiz1>0, 6,0
                                                        )))))#)
    })
  
  weights <-
    reactive({
      list(
        quiz1weight = round(input$quiz1 / 9 * 6, 2),
        quiz2weight = round(input$quiz2 / 9 * 6, 2),
        quiz3weight = round(input$quiz3 / 12 * 8, 2),
        #examweight  = round(input$exam / 100 * 30, 2),
        at1weight   = round(input$at1 / 100 * 20, 2),
        at2weight   = round(input$at2 / 100 * 20, 2)
      )
    })
  
  weightsum <- reactive({sum(
    weights()$quiz1weight,
    weights()$quiz2weight,
    weights()$quiz3weight,
    #weights()$examweight,
    weights()$at1weight,
    weights()$at2weight
  )})
  
  output$statement1 <- renderText({
    paste(
      "The entered grades amount to ",
      weightsum(),
      "% out of the ",
      denom(),
      "% available so far",
      ifelse(denom()>0, 
             paste(", which makes your running average ",
                   round(weightsum()/denom()*100, 2),
                   "%.", sep =""),
             "."), 
      sep ="" 
    )
  })
  output$statement2 <- renderText({
    if((input$target - weightsum())/(100-denom())<1){
      paste(
        "To exceed your target of ", 
        input$target, 
        "%, you will require an average of over ",
        round((input$target - weightsum())/(100-denom())*100, 2), 
        "% across the remaining assessments.")
    } else {
      "You cannot reach the specified target with the remaining marks available."
      }
  })  
  
  output$targetPlot <- renderPlot({
    
     df <- data.frame(Week = c(0,
                               4,
                               7, 
                               9, 
                               10, 
                               11),
                      Score = c(0,
                                weights()$quiz1, 
                                weights()$quiz1 + weights()$quiz2, 
                                weights()$quiz1 + weights()$quiz2 + weights()$at1,
                                weights()$quiz1 + weights()$quiz2 + weights()$at1 + weights()$quiz3, 
                                weights()$quiz1 + weights()$quiz2 + weights()$at1 + weights()$quiz3 + weights()$at2
                                )
     )
     df <- df[1:week(), ]
     
     ggplot(df, aes(Week, Score)) +
       scale_x_continuous(limits = c(0,13), breaks =  1:12, expand = c(0, 0)) +
       scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
       geom_line(cex = 1) +
       geom_hline(yintercept = input$target, colour = "red", linetype="dashed") +
       theme_classic()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

