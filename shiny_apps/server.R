#source('global.R')

shinyServer(function(input, output) {
  # You can access the value of the widget with input$slider1, e.g.
  output$myDataTable <- renderDataTable( {
    res <- PredictNextWord(input$user.input, prob.adj)
    return(data.frame(res))
    })
})
