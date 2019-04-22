
shinyServer(
  function(input, output){
    output$output.text <- renderText(input$id1)
    
  })