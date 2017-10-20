# Determine min/max values for sliders and default plots:
#get basic data
input <- df_basic()

Cmin <- min(input$C)
Cmax <- max(input$C)

Omin <- min(input$O)
Omax <- max(input$O)

Nmin <- min(input$N)
Nmax <- max(input$N)

Smin <- min(input$S)
Smax <- max(input$S)

Pmin <- min(input$P)
Pmax <- max(input$P)

HCmin <- min(input$HC)
HCmax <- max(input$HC)

OCmin <- min(input$OC)
OCmax <- max(input$OC)

NCmin <- min(input$NC)
NCmax <- max(input$NC)

mzmin <- min(input[,1])
mzmax <- max(input[,1])

errormin <- round(min(input$`error (ppm)`), 2)
errormax <- round(max(input$`error (ppm)`), 2)

intmin <- signif(min(input[,2]), 3)
intmax <- signif(max(input[,2]), 3)


