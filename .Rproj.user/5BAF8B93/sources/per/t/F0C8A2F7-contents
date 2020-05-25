library(shiny)
library(rhandsontable)
library(ggplot2)

#default table
DF = data.frame(type=rep("normal", 10), answers = rep(4L, 10), misc = rep(0L, 10))
ui <- fluidPage(
  titlePanel("Multiple Choice Test Simulator"),

  sidebarLayout(

    sidebarPanel(
      rHandsontableOutput("table"),
      sliderInput("reps", label = "Repetitions", min = 100, max = 10000, value = 1000),
      checkboxInput("truncation", label = "Truncate negative values?", value = TRUE),
      actionButton('submitTest', 'Submit Test'),
      downloadButton("downloadData", "Download Test"),
      span(style = "color:red", textOutput("errorMessage"))

    ),

    mainPanel(

      tabsetPanel(type = "tabs",
                  tabPanel("Results", tableOutput("results")),
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Raw Data", verbatimTextOutput("raw")),
                  tabPanel("About",
                           h3("MCTS is an R package / webapp focused on deriving statistics about multiple choice tests."),
                           "You can use the right mouse button to edit the table.", tags$p(), tags$i("type"), " - the type of test. \"normal\" refers to the standard question (1 mark for correct, 0 marks for incorrect).
                           \"neg\" refers to the standard question with harsh marking in which you lose a mark for an incorrect answer. \"multi\" refers to a question with multiple answers where you gain one mark for each correct answer.", tags$p(), tags$i("answers"), " - contains the amount of possible answers in the question.", tags$p(), tags$i("misc"), " - reserved exclusively for multiple choice question types; contains the amount of correct answers in the question.", tags$p(), "The repetitions slider refers to the amount of simulations ran under the given test. ", tags$p(), "The truncation of negative values means that test scores under zero will be set to zero. This is true by default."
                  )
      )
    )
  )
)

server <- function(input, output){

  #Update 'data' DF
  data <- reactiveValues(data=DF)

  #Render table with default data frame values
  output$table <- renderRHandsontable({
    rhandsontable(data$data) %>%
      hot_col(col = "type", type = "dropdown", source = c("normal", "neg", "multi"), strict = TRUE, default = "normal") %>%
      hot_col(col = "answers", default = "4") %>%
      hot_col(col = "misc", default = "0") %>%
      hot_validate_numeric(cols = 2, min = 1)
  })

  #If the 'Submit Test' button is pressed, call saveData()
  observeEvent(input$submitTest, saveData())

  saveData <- function(){
    data$data <- hot_to_r(input$table) #Update the data reactive variable with what is in the table

    if(sum(apply(data$data, 1, function(x){if (x[3] > x[2]) return(1) else return(0)} ) ) > 0){ #if one of the misc values exceeds answers values

      output$errorMessage <- renderText({ "ERROR: 'misc' cannot exceed 'answers'" })

    } else{
      output$errorMessage <- renderText({ "" })
      test <- mcts(data$data, reps = input$reps, truncation = input$truncation) #Create a test using mcts using the tabled data

      output$results <- renderTable({
        test$results
      })


      output$plot <- renderPlot({plot(test)})

      output$summary <- renderPrint({summary(test)})

      output$raw <- renderText({test$raw})
    }

  }

  output$downloadData <- downloadHandler(
    filename = "test.csv", content = function(file) {write.csv(hot_to_r(input$table), file, row.names = FALSE)}
  )

}


shinyApp(ui = ui, server = server)
