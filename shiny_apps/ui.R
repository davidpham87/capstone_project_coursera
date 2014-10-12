shinyUI(fluidPage(
  tags$h2('Coursera Word Prediction'),  
  
  fluidRow(
    p('This is the shiny project for text prediction'),
    p('It is not really hard to see how to use the app: Write a simple sentences in the text box below and then after a few seconds the app will give you a list of 20 words (ordered by the most probable) which might complete the input!'),
    p('The sources files can be found at'),
    a('Github Repo', href='https://github.com/davidpham87/capstone_project_coursera'),
    p("The pdf file explaining the Data processing and the Modelling part are published on the same website"),
    a("Reproducible report", href = "https://github.com/davidpham87/capstone_project_coursera/blob/master/report/word_prediction_report_DavidPham.pdf")  
    ),
  
                                        # Copy the line below to make a slider bar
  fluidRow(
    column(12, 
           textInput(inputId = 'user.input', 
                     label = 'User Argument. Text to predict.',
                     value = "I love")
           )
    ),
  hr(),   
  fluidRow(      
    column(width=12,
      tabsetPanel(
        tabPanel("Word Prediction", 
                 dataTableOutput("myDataTable")
                )
        )
      )
    )
  )
)       

# shinyapps::configureApp("coursera_capstone_data_science_project", size="xlarge")