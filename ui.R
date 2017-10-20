#load required libraries
library(shiny)
library(DT)
library(shinyjs)
library(CHNOSZ)
library(plotly)
library(Rdisop)


shinyUI(
fluidPage(
  
  titlePanel("Visualization of HR-MS data v2.1"),
  sidebarLayout(
    sidebarPanel(
      
      #file upload
      fileInput("file", label = "File Upload", multiple = FALSE),
     
     selectInput("FormulaCharge", "Formula type", choices = list("[M-H]-"=-1, "[M] neutral"=0, "[M+H]+"=1), selected = -1),

      uiOutput("sliders")

    ),

    mainPanel(
      wellPanel(
      tabsetPanel(
        tabPanel("Default",
              br(),
              plotOutput("plot", height = "533px", width = "800px"),
              br(),
              downloadButton("downloadPlot", "Export plots"),
              br(),
              br()),
        tabPanel("Custom Plots",
                 br(),
                 div(style="display:inline-block;vertical-align:top", selectInput("custom_p1x", "x-axis", choices = list("mz", "intensity", "z", "mass (theoret.)", "error (ppm)", "C", "H", "N", "O", "P", "S", "HC", "OC", "NC", "PC", "SC", "DBE", "AI", "Xc", "KMD", "AMD"), selected = "mz", width = "100px")),
                 div(style="display: inline-block;vertical-align:top; width: 20px",HTML("<br>")),
                 div(style="display:inline-block;vertical-align:top", selectInput("custom_p1y", "y-axis", choices = list("mz", "intensity", "z", "mass (theoret.)", "error (ppm)", "C", "H", "N", "O", "P", "S", "HC", "OC", "NC", "PC", "SC", "DBE", "AI", "Xc", "KMD", "AMD"), selected = "intensity", width = "100px")),
                 br(),
                 plotOutput("plotCustom", height = "533px", width = "800px"),
                 br(),
                 downloadButton("downloadCustom", "Export plots"),
                 br(),
                 br(),
                 radioButtons("radioCustom", label = "Style",
                              choices = list("default" = 1, "color" = 2, "scaling" = 3, "color & scaling" = 4), 
                              selected = 1),
                 br()
                 ),
        tabPanel("Custom Plotly",
                 br(),
                 div(style="display:inline-block;vertical-align:top", selectInput("customly_p1x", "x-axis", choices = list("mz", "intensity", "z", "mass (theoret.)", "error (ppm)", "C", "H", "N", "O", "P", "S", "HC", "OC", "NC", "PC", "SC", "DBE", "AI", "Xc", "KMD", "AMD"), selected = "mz", width = "100px")),
                 div(style="display: inline-block;vertical-align:top; width: 20px",HTML("<br>")),
                 div(style="display:inline-block;vertical-align:top", selectInput("customly_p1y", "y-axis", choices = list("mz", "intensity", "z", "mass (theoret.)", "error (ppm)", "C", "H", "N", "O", "P", "S", "HC", "OC", "NC", "PC", "SC", "DBE", "AI", "Xc", "KMD", "AMD"), selected = "intensity", width = "100px")),
                 br(),
                 plotlyOutput("plotCustomly", height = "533px", width = "800px"),
                 br()
        ),
        tabPanel("Quality Control",
                 br(),
                 plotOutput("plotQuality", height = "267px", width = "800px"),
                 br(),
                 uiOutput("sliderError"),
                 uiOutput("sliderAMD")),
        tabPanel("Advanced Settings",
                 br("Element valences:"),
                 div(style="display:inline-block",numericInput("valence_C", label = "C", value = 4, min = 0, max = 6, step = 1, width = "70px")),
                 div(style="display:inline-block",numericInput("valence_H", label = "H", value = 1, min = 0, max = 6, step = 1, width = "70px")),
                 div(style="display:inline-block",numericInput("valence_O", label = "O", value = 2, min = 0, max = 6, step = 1, width = "70px")),
                 div(style="display:inline-block",numericInput("valence_N", label = "N", value = 3, min = 0, max = 6, step = 1, width = "70px")),
                 div(style="display:inline-block",numericInput("valence_S", label = "S", value = 2, min = 0, max = 6, step = 1, width = "70px")),
                 div(style="display:inline-block",numericInput("valence_P", label = "P", value = 5, min = 0, max = 6, step = 1, width = "70px")),
                 br(),
                 br("Aromaticity equivalent (Xc)"),
                 div(style="display:inline-block",numericInput("Xc_m", label = "m", value = 0, min = 0.5, max = 1, step = 0.5, width = "70px")),
                 div(style="display:inline-block",numericInput("Xc_n", label = "n", value = 0, min = 0, max = 1, step = 0.5, width = "70px")),
                 br(),
                 br("Kendrick mass defect:"),
                 div(style="display:inline-block",textInput("KMD_IUPACmass", label = "IUPAC mass used to calculate KMD:", value = 14.01565, width = "120px"))
        ),
        tabPanel("About"))),
      wellPanel(
      tabsetPanel(
        tabPanel("Data Table",
                 br(),
                 DT::dataTableOutput("liveTable"),
                 br(),
                 downloadButton("downloadData", "Download table")),
        tabPanel("Plot Settings",
                 br(),
                 checkboxInput("make_interactive", "activate interactive functions"),
                 br(),
                 sliderInput("bubblesize", "Bubble size range", min = 0, max = 8, value = c(0.001, 8), step = 0.001, width = "400px"),
                 br("Colors:"),
                 br(),
                 textInput("transparency", label = "Transparency", value = "0.5", width = "100px"),
                 div(style="display:inline-block",textInput("color_CHO", label = "CHO", value = "#00008B", width = "100px")),
                 div(style="display:inline-block",textInput("color_CHNO", label = "CHNO", value = "#FF7F00", width = "100px")),
                 div(style="display:inline-block",textInput("color_CHOS", label = "CHOS", value = "#228B22", width = "100px")),
                 div(style="display:inline-block",textInput("color_CHOP", label = "CHOP", value = "#EE1289", width = "100px")),
                 div(style="display:inline-block",textInput("color_CHNOS", label = "CHNOS", value = "#CD0000", width = "100px")),
                 div(style="display:inline-block",textInput("color_CHNOP", label = "CHNOP", value = "#B23AEE", width = "100px")),
                 div(style="display:inline-block",textInput("color_CHNOSP", label = "CHNOSP", value = "#0000CD", width = "100px")),
                 div(style="display:inline-block",textInput("color_others", label = "others", value = "#AAAAAA", width = "100px"))
                 ),
        tabPanel("Import/Export Settings",
                 br("Save settings"),
                 br(),
                 div(style="display:inline-block",dateInput("export.date", label = "Date", value = Sys.Date())),
                 div(style="display:inline-block",textInput("export.text", label = "Description", value = "", width = "300px")),
                 br(),
                 tags$head(tags$script(src = "message-handler.js")),
                 downloadButton("SaveUserSettings", "Save settings"),
                 hr()
                 )
      )
    )
      )
    )
    )
)

