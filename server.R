#load required libraries
library(shiny)
library(DT)
library(ggplot2)
library(gridExtra)
library(plotly)
library(shinyjs)
library(CHNOSZ)
library(Rdisop)






# Define server logic
function(input, output, session) {
  
  data("thermo")

  #==============================================================
  # Read basic dataframe and apply precalculations
  #==============================================================
  
  df_basic <- reactive({
    
   
    #check if file has been uploaded
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    #Upload file
    input <- read.table(inFile$datapath, header = TRUE, sep = "\t", check.names = FALSE)
    
    # Apply precalculations (elemental composition, molecular characteristics):
    Settings <- UserSettings1()
    
    source("Precalculations.R", local = TRUE)
    
    
    
    input
  })
  

  

  
  
  #==============================================================
  #create sliders for elemental ratios
  #==============================================================
  
  output$sliders <- renderUI({
    
    source("minmax.R", local = TRUE)
    
       tabsetPanel(
                 tabPanel("Basic Filter",
                          br(),
                          #sliders for filtering
                          sliderInput("hcSlider", label = "H/C ratio",
                                      min = HCmin,
                                      max = HCmax,
                                      value = c(HCmin, HCmax)),
                          
                          sliderInput("ocSlider", label = "O/C ratio",
                                      min = OCmin,
                                      max = OCmax,
                                      value = c(OCmin, OCmax)),
                          
                          sliderInput("ncSlider", label = "N/C ratio",
                                      min = NCmin,
                                      max = NCmax,
                                      value = c(NCmin, NCmax)),
                          
                          sliderInput("mzSlider", label = "m/z",
                                      min = floor(mzmin),
                                      max = ceiling(mzmax),
                                      value = c(mzmin, mzmax)),
                          
                          sliderInput("intSlider", label = "Intensity",
                                      min = intmin,
                                      max = intmax,
                                      value = c(intmin, intmax),
                                      round = TRUE)
                 ),
                 
                 tabPanel("Atom Filter",
                          #sliders for filtering
                          sliderInput("cSlider", label = "Number of C Atoms",
                                      min = Cmin,
                                      max = Cmax,
                                      value = c(Cmin, Cmax)),
                          
                          sliderInput("oSlider", label = "Number of O Atoms",
                                      min = Omin,
                                      max = Omax,
                                      value = c(Omin, Omax)),
                          
                          sliderInput("nSlider", label = "Number of N Atoms",
                                      min = Nmin,
                                      max = Nmax,
                                      value = c(Nmin, Nmax)),
                          
                          sliderInput("sSlider", label = "Number of S Atoms",
                                      min = Smin,
                                      max = Smax,
                                      value = c(Smin, Smax)),
                          
                          sliderInput("pSlider", label = "Number of P Atoms",
                                      min = Pmin,
                                      max = Pmax,
                                      value = c(Pmin, Pmax))
                 ),
                 tabPanel("Performance Filter",
                          #sliders for filtering
                          sliderInput("errorSlider", label = "Error range", min = errormin, max = errormax, value = c(errormin, errormax), width = "400px", round = -2),
                          sliderInput("AMDSlider", label = "Mass defect range", min = 0, max = 1, value = c(0, 1), width = "400px", round = -1)
                 )
               )
               
      
  })
  
  
  
  
  
  #==============================================================
  #create dataframe containing all slider inputs
  #==============================================================
  df_sliders <- reactive({

    df_sliders <- data.frame(type = character(0),
                             min = numeric(0),
                             max = numeric(0))

    #get C atom slider [1,]
    df_sliders <- rbind.data.frame(df_sliders, cbind.data.frame(type = "C",
                                                                min = input$cSlider[1],
                                                                max = input$cSlider[2]))

    #get O atom slider [2,]
    df_sliders <- rbind.data.frame(df_sliders, cbind.data.frame(type = "O",
                                                                min = input$oSlider[1],
                                                                max = input$oSlider[2]))

    #get N atom slider [3,]
    df_sliders <- rbind.data.frame(df_sliders, cbind.data.frame(type = "N",
                                                                min = input$nSlider[1],
                                                                max = input$nSlider[2]))

    #get S atom slider [4,]
    df_sliders <- rbind.data.frame(df_sliders, cbind.data.frame(type = "S",
                                                                min = input$sSlider[1],
                                                                max = input$sSlider[2]))

    #get P atom slider [5,]
    df_sliders <- rbind.data.frame(df_sliders, cbind.data.frame(type = "P",
                                                                min = input$pSlider[1],
                                                                max = input$pSlider[2]))

    #get H/C ratio slider [6,]
    df_sliders <- rbind.data.frame(df_sliders, cbind.data.frame(type = "HC",
                                                                min = input$hcSlider[1],
                                                                max = input$hcSlider[2]))

    #get O/C ratio slider [7,]
    df_sliders <- rbind.data.frame(df_sliders, cbind.data.frame(type = "OC",
                                                                min = input$ocSlider[1],
                                                                max = input$ocSlider[2]))

    #get N/C ratio slider [8,]
    df_sliders <- rbind.data.frame(df_sliders, cbind.data.frame(type = "NC",
                                                                min = input$ncSlider[1],
                                                                max = input$ncSlider[2]))
 
    #get m/z ratio slider [9,]
    df_sliders <- rbind.data.frame(df_sliders, cbind.data.frame(type = "mz",
                                                                min = input$mzSlider[1],
                                                                max = input$mzSlider[2]))
    
    # Quality control sliders
    #get AMD ratio slider [10,]
    df_sliders <- rbind.data.frame(df_sliders, cbind.data.frame(type = "AMDSlider",
                                                                min = input$AMDSlider[1],
                                                                max = input$AMDSlider[2]))
    
    #get Error ratio slider [11,]
    df_sliders <- rbind.data.frame(df_sliders, cbind.data.frame(type = "errorSlider",
                                                                min = input$errorSlider[1],
                                                                max = input$errorSlider[2]))
    
    #get Intensity slider [12,]
    df_sliders <- rbind.data.frame(df_sliders, cbind.data.frame(type = "intSlider",
                                                                min = input$intSlider[1],
                                                                max = input$intSlider[2]))
    
    
    
    
    
    
    
    df_sliders
  })

  

  #==============================================================
  #Create df_live table 
  #==============================================================
  
  df_live <- reactive({
    basic <- df_basic()
    sliders <- df_sliders()
    samplename <- input$selected_sample
   
    
    basic <- subset(basic, basic$C >= sliders[1,2] & basic$C <= sliders[1,3] &
                      basic$O >= sliders[2,2] & basic$O <= sliders[2,3] &
                      basic$N >= sliders[3,2] & basic$N <= sliders[3,3] &
                      basic$S >= sliders[4,2] & basic$S <= sliders[4,3] &
                      basic$P >= sliders[5,2] & basic$P <= sliders[5,3] &
                      basic$HC >= sliders[6,2] & basic$HC <= sliders[6,3] &
                      basic$OC >= sliders[7,2] & basic$OC <= sliders[7,3] &
                      basic$NC >= sliders[8,2] & basic$NC <= sliders[8,3] &
                      basic[,1] >= sliders[9,2] & basic[,1] <= sliders[9,3] &
                      basic$AMD >= sliders[10,2] & basic$AMD <= sliders[10,3] &
                      basic$`error (ppm)` >= sliders[11,2] & basic$`error (ppm)` <= sliders[11,3] &
                      basic[,2] >= sliders[12,2] & basic[,2] <= sliders[12,3]
                      )
    basic
  })
  
  output$liveTable <- DT::renderDataTable({
    print(df_live())
    
  }, options = list(scrollX = TRUE), selection = "single")
  
  # Download possibility of df_live
  output$downloadData <- downloadHandler(
    filename = function() { 
      "data.txt"
    },
    
    content = function(file) {
      write.table(x = df_live(),
                  file = file,
                  quote = FALSE, sep = "\t", row.names = FALSE)
    }
  ) 
  
  
  #==============================================================
  # User settings
  #==============================================================
  UserSettings1 <- reactive({
    PlotSettings <- list(
      "colors"=c("CHO"=input$color_CHO,
                 "CHNO"=input$color_CHNO,
                 "CHOS"=input$color_CHOS,
                 "CHOP"=input$color_CHOP,
                 "CHNOS"=input$color_CHNOS,
                 "CHNOP"=input$color_CHNOP,
                 "CHNOSP"=input$color_CHNOSP,
                 "nd"=input$color_others),
      "interactive"=input$make_interactive,
      "bubble_size"=input$bubblesize,
      "transparency"=input$transparency,
      "valenceH"=input$valence_H,
      "valenceC"=input$valence_C,
      "valenceO"=input$valence_O,
      "valenceN"=input$valence_N,
      "valenceS"=input$valence_S,
      "valenceP"=input$valence_P,
      "FormulaCharge"=as.numeric(input$FormulaCharge),
      "Xc_m"=input$Xc_m,
      "Xc_n"=input$Xc_n,
      "KMD_IUPAC"=as.numeric(input$KMD_IUPACmass),
      "custom_x"=input$custom_p1x,
      "custom_y"=input$custom_p1y,
      "custom_style"=input$radioCustom,
      "customly_x"=input$customly_p1x,
      "customly_y"=input$customly_p1y
      )
    
    PlotSettings
    
  })
 
# Daniel: 18.Nov. 2016, Settings Export Dummy DataFrame
# Export User Settings. Aktuell nur Slider settings. In Zukunft sollten auch andere Settings wie Colors,... aufgenommen werden,
# aktuell jedoch diese Settings in Listen gespeichert. Erst nachdem klar ist, welches Format der Setting-Speicherung (Liste, DF,...)
# am Besten geeignet ist, restliche betroffene Bereiche umschreiben.
  
  UserSettings <- reactive({ 
    # get slider settings
    Settings <- df_sliders()
    
    Settings
    
  })
  


  # Save User settings
  output$SaveUserSettings <- downloadHandler(
    filename = function() { 
      paste(input$export.date, input$export.text, ".txt", sep = "")
    },
    
    content = function(file) {
      write.table(x = UserSettings(),
                  file = file,
                  quote = FALSE, sep = "\t", row.names = FALSE)
    }
  ) 
  
# End Export Dummy  
  
  
  #==============================================================
  # Plots
  #==============================================================

  #separate output and generation of plot
  plotInput<-function(){
    
    #read live data
    VK.plot <- df_live()
    PlotSettings <- UserSettings1()
    row_selected <- input$liveTable_rows_selected
    
    Int.sum <- sum(as.numeric(VK.plot[,2]))
    VK.plot$relInt <- VK.plot[,2] / Int.sum
   
    
    # get min and max default values:
    source("minmax.R", local = TRUE)
    
    # generate plots:
    source("plot_default.R", local = TRUE)
  }
  
  plotQuality<-function(){
    
    #read live data
    VK.plot <- df_live()
    PlotSettings <- UserSettings1()
    row_selected <- input$liveTable_rows_selected
    
    # get min and max default values:
    source("minmax.R", local = TRUE)
    
    # generate plots:
    source("plot_quality.R", local = TRUE)
  }
  
             
  output$plot <- renderPlot({
    print(plotInput())
    })
  
  
  output$plotQuality <- renderPlot({
    
    print(plotQuality())
    
  })
  
  # Allow user to download the plot
  # default plots
  output$downloadPlot <- downloadHandler(
    filename = function() {
      "Plots.svg"
    },
    
    content = function(file) {
      svg(file = file, width = 7, height = 5.522)
      print(plotInput())
      dev.off()
    }
  )
  
  
  # Custom plots:
  
  plotCustom<-function(){
    
    #read live data
    VK.plot <- df_live()
    PlotSettings <- UserSettings1()
    row_selected <- input$liveTable_rows_selected
    
    Int.sum <- sum(as.numeric(VK.plot[,2]))
    VK.plot$relInt <- VK.plot[,2] / Int.sum
    
    # get min and max default values:
    source("minmax.R", local = TRUE)
    
    # generate plots:
    source("plot_custom.R", local = TRUE)
  }
  
  output$plotCustom <- renderPlot({
    print(plotCustom())
    })
    
  
  
  # Download custom plot
  output$downloadCustom <- downloadHandler(
    filename = function() {
      "Plot.svg"
    },
    
    content = function(file) {
      svg(file = file, width = 7, height = 5.522)
      print(plotCustom())
      dev.off()
    }
  )
  
  
  
  
  # Custom plotLY: Interactive Plot!!
  
  
  
  output$plotCustomly <- renderPlotly({
   
      
      #read live data
      VK.plot <- df_live()
      PlotSettings <- UserSettings1()
      row_selected <- input$liveTable_rows_selected
      
      Int.sum <- sum(as.numeric(VK.plot[,2]))
      VK.plot$relInt <- VK.plot[,2] / Int.sum
      
      # get min and max default values:
      source("minmax.R", local = TRUE)
      
      pc <- plot_ly(data = VK.plot, x = VK.plot[,PlotSettings$customly_x], y = VK.plot[,PlotSettings$customly_y], color = ~composition, colors = PlotSettings$colors, alpha = as.numeric(PlotSettings$transparency), size = ~relInt, sizes = PlotSettings$bubble_size * 50,
                      text = ~paste('Formula: ', formula, '</br>Charge: ', z, '</br>Error (ppm): ', `error (ppm)`)) %>%
          layout(xaxis = list(title = PlotSettings$customly_x, linecolor = toRGB("black"), linewidth = 1, showgrid = FALSE), yaxis = list(title = PlotSettings$customly_y, linecolor = toRGB("black"), linewidth = 1, showgrid = FALSE))
      
      
      
    
  })
  
  
  
  
                                                                                                          
}
