shinyUI(fluidPage(
  tags$h2('Coursera Word Prediction'),  
  
  fluidRow(
    p('This is the shiny project for text prediction'),
    p('It is not really hard to see how to use the app: Write a simple sentences in the text box below and then after a few seconds the app will give you a list of 20 words (ordered by the most probable) which might complete the input!'),
    p('The sources files can be found at'),
    a('Github Repo', 'https://github.com/davidpham87/capstone_project_coursera'),
    p("The two *.rmd files explaining the Data processing and the Modelling part are published on the same website"),
    a("Reproducible report", href = "http://rpubs.com/davidpham87/19545")  
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
