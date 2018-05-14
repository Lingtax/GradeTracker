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
               textOutput("quiz1weight"),
               textOutput("quiz2weight"),
               textOutput("quiz3weight"),
               textOutput("AT1weight"),
               textOutput("AT2weight"),
               
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
      a("this github repository", href = "https://github.com/Lingtax/PlanningProgressDashboard/issues/new")
    )
  )
)

         
# Define server logic 
server <- function(input, output) {
   
  weights <-  reactive({})
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

