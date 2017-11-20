library(shiny)
require(ggseqlogo)
# rsconnect::deployApp('~/Development/ggseqlogo/shiny/ggseqlogo/', account = 'omarwagih')

do_plot <- function(input){
  
  seqs = strsplit(input$txt, '\n')[[1]]
  stack_width = input$stack_width
  if(stack_width <= 0) stack_width = 1e-10
  
  # Draw sequence logo
  ggseqlogo(seqs, col_scheme=input$col_scheme, 
            font=input$font, method=input$method, 
            stack_width=stack_width, 
            rev_stack_order=input$rev_stack_order, 
            seq_type=input$seq_type)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Draw plot
  observe({
    output$distPlot <- renderPlot({
      do_plot(input)
    }, height = as.numeric(input$plot_height), width = as.numeric(input$plot_width))
  })
  
  # Download plot filename, content
  output$download_plot <- downloadHandler(
    filename =  function() {
      paste("ggseqlogo", input$download_format, sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      # if(input$download_format == "png")
      #   png(file) # open the png device
      # else
      #   pdf(file) # open the pdf device
      
      ggsave(file, do_plot(input), device = input$download_format, 
             width = as.numeric(input$plot_width) / 96, 
             height = as.numeric(input$plot_height) /96, units = 'in')
      # print(do_plot(input))
      # dev.off()  # turn the device off
      
    } 
  )
  
})
