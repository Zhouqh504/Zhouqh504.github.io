#The shiny app was created by zhouqh in 2023.5.13 email:zhouqh20010504@163.com
#install.packages("colourpicker")

#if (!require('shiny')) install.packages('shiny')
#if (!require('CMplot')) install.packages("CMplot", version = "4.3.1")
#if (!require('colourpicker')) install.packages('colourpicker')
#if (!require('htmltools')) install.packages('htmltools')



library(shiny)
library(CMplot)
library(colourpicker)
library(htmltools)


# Define UI for Shiny app
ui <- fluidPage(

  # App title
  titlePanel("Rectangular Manhattan Plot"),

  # Sidebar panel
  sidebarPanel(

    # File upload
    fileInput("datafile", "Upload data file"),
	

    # Select colors
    colourInput("color1", "Select Manha color 1:", value = "#666666"),
    colourInput("color2", "Select Manha color 2:", value = "#999999"),
	colourInput("color", "Select QQ-plot color:", value = "#999999"),

    # Select band
    numericInput("band", "Enter band value:", value = 0),

    # Select log10
    checkboxInput("log10", "Use log10 scale?", value = TRUE),
	
	# Attach chromosome density
    checkboxInput("chr.den.col", "Attach chromosome density?", value = FALSE),

    # Select ylim
    numericInput("ylim_min", "Enter y-axis lower limit:", value = 0),
    numericInput("ylim_max", "Enter y-axis upper limit:", value = 30),

    # Select threshold options
    checkboxInput("use_threshold", "Use threshold value(s)?", value = FALSE),
    conditionalPanel(
      condition = "input.use_threshold == true",

      textInput("thresholds", "Enter threshold value(s):", value = "0.01,0.05"),
      # Input threshold colors
      textInput("threshold_colors", "Enter threshold color(s):", value = "red,green"),

      # Input threshold linetypes
      textInput("threshold_linetypes", "Enter threshold linetype(s)_1 (solid),2 (dashed):", value = "1,2"),

      # Input threshold linewidths
      textInput("threshold_linewidths", "Enter threshold linewidth(s):", value = "2,2"),

      checkboxInput("add_significant", "Add significant points?", value = FALSE),

      conditionalPanel(
        condition = "input.add_significant == true",
        sliderInput("p_value", "Significance threshold:", min = 0, max = 1, value = 0.05, step = 0.01),
        numericInput("signal_cex", "Signal size:", value = 1.5),
        textInput("signal_col", "Signal color:", value = "red"),
        numericInput("signal_pch", "Signal point type:", value = 19)
      )
    ),
	
	# 在UI中定义sliderInput
    sliderInput(inputId = "plotWidth", label = "Plot Width",
            min = 0, max = 100, value = 30),

    sliderInput(inputId = "plotHeight", label = "Plot Height",
            min = 0, max = 100, value = 30),
			
	#
	checkboxInput("downloadPlot", "download fig in work dir",value = FALSE)
    
	#下载保存图
    #downloadButton("downloadPlot", "Download plot")
    # Generate button
    #actionButton("generate", "Generate CMplot")
  ),

  # Main panel

      mainPanel(
      tabsetPanel(
        tabPanel("Rectangular-Manhattan", 
                 plotOutput("cmplot"),
                 plotOutput("cmplot_file")

        ),
        tabPanel("Q-Q plot",
                 plotOutput("qq_plot"),
                 plotOutput("qq_plot_file")

      )
    )
)
)
# Define server logic for Shiny app
server <- function(input, output) {

  # Load data
  data <- reactive({
    req(input$datafile)
    read.table(input$datafile$datapath, header = TRUE)
  })
  

	
	output$cmplot <- renderPlot({
	
	# 在这里定义chr.den.col变量
   chr.den.col <- if(input$chr.den.col) c("darkgreen", "yellow", "red") else NA
	
	   # Convert threshold input to vector of values
    thresholds <- if(input$use_threshold) as.numeric(strsplit(input$thresholds, ",")[[1]])
  
    # Convert threshold color input to vector of values
    threshold_colors <- if(input$use_threshold) unlist(strsplit(input$threshold_colors, ","))
  
    # Convert threshold linetype input to vector of values
    threshold_linetypes <- if(input$use_threshold) as.numeric(strsplit(input$threshold_linetypes, ",")[[1]])
  
    # Convert threshold linewidth input to vector of values
    threshold_linewidths <- if(input$use_threshold) as.numeric(strsplit(input$threshold_linewidths, ",")[[1]])

      signal.cex = if(input$add_significant) input$signal_cex else NA
      signal.col = if(input$add_significant) input$signal_col else NA
      signal.pch = if(input$add_significant) input$signal_pch else NA
	  
    # Generate plot with or without threshold lines based on user input
    if (input$use_threshold) {
      CMplot(data(), plot.type = "m", band = input$band, LOG10 = input$log10, 
             col = c(input$color1, input$color2), 
             threshold = thresholds, threshold.col = threshold_colors, 
             threshold.lty = threshold_linetypes, threshold.lwd = threshold_linewidths,
             amplify = TRUE, signal.cex = signal.cex, signal.col = signal.col, signal.pch = signal.pch,
			 width = input$plotWidth, height = input$plotHeight,ylim=c(input$ylim_min,input$ylim_max),
			 chr.den.col=chr.den.col,
			 file.output=FALSE)
			 
    } else {
      CMplot(data(), plot.type = "m", band = input$band, LOG10 = input$log10, 
             col = c(input$color1, input$color2),
			 width = input$plotWidth, height = input$plotHeight,ylim=c(input$ylim_min,input$ylim_max),
			 chr.den.col=chr.den.col,
			 file.output=FALSE)

    }
  })

  output$cmplot_file <- renderPlot({
      
	 chr.den.col <- if(input$chr.den.col) c("darkgreen", "yellow", "red") else NA
     # Convert threshold input to vector of values
    thresholds <- if(input$use_threshold) as.numeric(strsplit(input$thresholds, ",")[[1]])
  
    # Convert threshold color input to vector of values
    threshold_colors <- if(input$use_threshold) unlist(strsplit(input$threshold_colors, ","))
  
    # Convert threshold linetype input to vector of values
    threshold_linetypes <- if(input$use_threshold) as.numeric(strsplit(input$threshold_linetypes, ",")[[1]])
  
    # Convert threshold linewidth input to vector of values
    threshold_linewidths <- if(input$use_threshold) as.numeric(strsplit(input$threshold_linewidths, ",")[[1]])

      signal.cex = if(input$add_significant) input$signal_cex else NA
      signal.col = if(input$add_significant) input$signal_col else NA
      signal.pch = if(input$add_significant) input$signal_pch else NA
	  
  if (input$downloadPlot) {
    # Generate plot with or without threshold lines based on user input
    if (input$use_threshold) {
      CMplot(data(), plot.type = "m", band = input$band, LOG10 = input$log10, 
                  col = c(input$color1, input$color2), 
                  threshold = thresholds, threshold.col = threshold_colors, 
                  threshold.lty = threshold_linetypes, threshold.lwd = threshold_linewidths,
                  amplify = TRUE, signal.cex = signal.cex, signal.col = signal.col, signal.pch = signal.pch,
                  width = input$plotWidth, height = input$plotHeight, ylim = c(input$ylim_min, input$ylim_max),
				  chr.den.col=chr.den.col,
				  file=c("jpg","pdf","tiff"))
    } else {
      CMplot(data(), plot.type = "m", band = input$band, LOG10 = input$log10, 
                  col = c(input$color1, input$color2),
                  width = input$plotWidth, height = input$plotHeight, ylim = c(input$ylim_min, input$ylim_max),
				  chr.den.col=chr.den.col,
				   file=c("jpg","pdf","tiff"))
    }
	}})
	
	#qqplot
	output$qq_plot <- renderPlot({
	#qqplot
	     # Convert threshold input to vector of values
    thresholds <- if(input$use_threshold) as.numeric(strsplit(input$thresholds, ",")[[1]])
  
    # Convert threshold color input to vector of values
    threshold_colors <- if(input$use_threshold) unlist(strsplit(input$threshold_colors, ","))
  
    # Convert threshold linetype input to vector of values
    threshold_linetypes <- if(input$use_threshold) as.numeric(strsplit(input$threshold_linetypes, ",")[[1]])
  
    # Convert threshold linewidth input to vector of values
    threshold_linewidths <- if(input$use_threshold) as.numeric(strsplit(input$threshold_linewidths, ",")[[1]])
	
	CMplot(data(),plot.type="q",box=FALSE,dpi=300,
    conf.int=TRUE,conf.int.col=NULL,
    file.output=FALSE,verbose=TRUE,
	width = input$plotWidth, height = input$plotHeight,
	ylim = c(input$ylim_min, input$ylim_max),
	threshold.lty = threshold_linetypes, threshold.lwd = threshold_linewidths,
	threshold = thresholds, threshold.col = threshold_colors,
	col=input$color
	)
	
	})
	
	##qq_plot_file
    output$qq_plot_file <- renderPlot({
     # Convert threshold input to vector of values
    thresholds <- if(input$use_threshold) as.numeric(strsplit(input$thresholds, ",")[[1]])
  
    # Convert threshold color input to vector of values
    threshold_colors <- if(input$use_threshold) unlist(strsplit(input$threshold_colors, ","))
  
    # Convert threshold linetype input to vector of values
    threshold_linetypes <- if(input$use_threshold) as.numeric(strsplit(input$threshold_linetypes, ",")[[1]])
  
    # Convert threshold linewidth input to vector of values
    threshold_linewidths <- if(input$use_threshold) as.numeric(strsplit(input$threshold_linewidths, ",")[[1]])

      signal.cex = if(input$add_significant) input$signal_cex else NA
      signal.col = if(input$add_significant) input$signal_col else NA
      signal.pch = if(input$add_significant) input$signal_pch else NA
	  
    if (input$downloadPlot) {
    # Generate plot with or without threshold lines based on user input
	CMplot(data(),plot.type="q",box=FALSE,dpi=300,
    conf.int=TRUE,conf.int.col=NULL,
    file.output=TRUE,verbose=TRUE,
	width = input$plotWidth, height = input$plotHeight,
	ylim = c(input$ylim_min, input$ylim_max),
	threshold.lty = threshold_linetypes, threshold.lwd = threshold_linewidths,
	threshold = thresholds, threshold.col = threshold_colors,
	file=c("jpg","pdf","tiff"),
	col=input$color
	)

	}})
}

shinyApp(ui, server)
