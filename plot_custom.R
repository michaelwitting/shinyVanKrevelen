df.selection <- VK.plot[row_selected,]

if(PlotSettings$custom_style == 1){
  pc <- ggplot(data = VK.plot, aes_string(x = VK.plot[,PlotSettings$custom_x], y = VK.plot[,PlotSettings$custom_y])) +
    geom_point(shape = 16, alpha = PlotSettings$transparency) +
    geom_vline(xintercept = df.selection[,PlotSettings$custom_x]) +
    geom_hline(yintercept = df.selection[,PlotSettings$custom_y]) +
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
  plot(pc)
} 



if(PlotSettings$custom_style == 2){
  pc <- ggplot(data = VK.plot, aes_string(x = VK.plot[,PlotSettings$custom_x], y = VK.plot[,PlotSettings$custom_y])) +
    geom_point(shape = 16, aes(colour = VK.plot$composition), alpha = PlotSettings$transparency) +
    geom_vline(xintercept = df.selection[,PlotSettings$custom_x]) +
    geom_hline(yintercept = df.selection[,PlotSettings$custom_y]) +
    scale_color_manual(values = PlotSettings$colors) +
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
  plot(pc)
}  



if(PlotSettings$custom_style == 3){
  pc <- ggplot(data = VK.plot, aes_string(x = VK.plot[,PlotSettings$custom_x], y = VK.plot[,PlotSettings$custom_y])) +
    geom_point(shape = 16, aes(size = VK.plot$relInt), alpha = PlotSettings$transparency) +
    geom_vline(xintercept = df.selection[,PlotSettings$custom_x]) +
    geom_hline(yintercept = df.selection[,PlotSettings$custom_y]) +
    scale_size(range = PlotSettings$bubble_size) +
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
  plot(pc)
}  







if(PlotSettings$custom_style == 4){
pc <- ggplot(data = VK.plot, aes_string(x = VK.plot[,PlotSettings$custom_x], y = VK.plot[,PlotSettings$custom_y])) +
  geom_point(shape = 16, aes(size = VK.plot$relInt, colour = VK.plot$composition), alpha = PlotSettings$transparency) +
  geom_vline(xintercept = df.selection[,PlotSettings$custom_x]) +
  geom_hline(yintercept = df.selection[,PlotSettings$custom_y]) +
  scale_size(range = PlotSettings$bubble_size) +
  scale_color_manual(values = PlotSettings$colors) +
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
plot(pc)
}  
  
