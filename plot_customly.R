
  pc <- ggplot(data = VK.plot, aes_string(x = VK.plot[,PlotSettings$custom_x], y = VK.plot[,PlotSettings$custom_y])) +
    geom_point(shape = 16, alpha = PlotSettings$transparency) +
    theme_bw() +
    xlab(paste(PlotSettings$custom_x)) +
    ylab(paste(PlotSettings$custom_y)) +
    theme(legend.position = "none", legend.key = element_blank()) +
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 14),
          legend.title = element_blank(),
          legend.text = element_text(size = 10, face = "bold"))
  ggplotly(pc)




