library(shiny)
require(ggplot2)
require(ggseqlogo)

sample_seqs = "ATT\nTAA\nTTT"
sample_seqs = "AATTGTGGTTA\nATCTGTGGTTA\nAATTGTGGTAA\nTTCTGCGGTTA\nAATTGCGGTAA\nTATTGCGGTTT\nTATTGTGGTAG\nTTTTGTGGTAG\nATATGTGGTAA\nAACTGCGGTTG\nGACTGTGGTTG\nCTTTGCGGTTA\nATCTATGGTTA\nCATTGCGGTTT\nTATTGTGGCTA\nTCTTGTGGTAT\nTGCTGCGGTTA\nTTTTGTGGTCT\nGCTTGTGGTTT\nAAATGTGGTAC\nTTATGAGGTTA\nGTCTGCGGTAT\nAAAAGTGGTTA\nTTGTATGGTTT\nAATTGAGGTCC\nTTTCTTGGTTA"


# Define UI 
shinyUI(fluidPage(
  
  # Application title
  #titlePanel("ggseqlogo webserver"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    
    
    # Show a plot of the generated distribution
    mainPanel(
      img(src='logo.svg', align = "left", style='height:60px; margin-bottom:50px;'),
      plotOutput("distPlot")
    ),
    
    sidebarPanel(style = "overflow-y:scroll; max-height: 600px; margin-top:35px",
      textAreaInput("txt", "Data input", sample_seqs, height = 200),
      selectInput("seq_type", "Sequence type:", c('auto', 'dna', 'rna', 'aa')),
      selectInput("method", "Method:", c('bits', 'probability')),
      br(),
      selectInput("col_scheme", "Color scheme:", list_col_schemes(F)),
      selectInput("font", "Font:", list_fonts(F), selected = 'roboto_medium'),
      br(), 
      sliderInput("stack_width", "Stack width:", min = 0, max = 1, value = 0.95),
      checkboxInput('rev_stack_order', label = 'Reverse stack order', value = F),
      br(),
      sliderInput("plot_height", "Plot height:", min = 300, max = 1000, value = 300),
      sliderInput("plot_width", "Plot width:", min = 300, max = 1000, value = 600),
      selectInput("download_format", "Download format:", c('pdf', 'png')),
      downloadButton("download_plot", "Download plot")
      
    )
    
    
  )
))



