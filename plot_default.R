df.selection <- VK.plot[row_selected,]

#create default plots
p1 <- ggplot(data = VK.plot, aes_string(x = VK.plot[,1], y = VK.plot[,2])) +
  geom_segment(xend = VK.plot[,1], yend = 0) +
  geom_point(data = df.selection, aes(x = df.selection[,1], y = df.selection[,2])) +
  scale_x_continuous(limits = c(mzmin, mzmax)) +
  theme_bw() +
  theme(legend.position = "bottom", legend.key = element_blank()) +
  xlab("\nm/z") +
  ylab("Intensity\n") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 10, face = "bold"))


p2 <- ggplot(data = VK.plot, aes_string(x = VK.plot$OC, y = VK.plot[,1])) +
  geom_point(shape = 16, aes(size = VK.plot$relInt, colour = VK.plot$composition), alpha = PlotSettings$transparency) +
  geom_vline(xintercept = df.selection$OC) +
  geom_hline(yintercept = df.selection[,1]) +
  scale_color_manual(values = PlotSettings$colors) +
  scale_size(range = PlotSettings$bubble_size) + 
  scale_y_continuous(limits = c(mzmin, mzmax)) +
  scale_x_continuous(limits = c(OCmin, OCmax)) +
  theme_bw() +
  theme(legend.position = "none", legend.key = element_blank()) +
  xlab("\nO/C") +
  ylab("m/z\n") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 10, face = "bold"))


p3 <- ggplot(data = VK.plot, aes_string(x = VK.plot[,1], y = VK.plot$HC)) +
  geom_point(shape = 16, aes(size = VK.plot$relInt, colour = VK.plot$composition), alpha = PlotSettings$transparency) +
  geom_vline(xintercept = df.selection[,1]) +
  geom_hline(yintercept = df.selection$HC) +
  scale_color_manual(values = PlotSettings$colors) +
  scale_size(range = PlotSettings$bubble_size) +
  scale_y_continuous(limits = c(HCmin, HCmax)) +
  scale_x_continuous(limits = c(mzmin, mzmax)) +
  theme_bw() +
  theme(legend.position = "none", legend.key = element_blank()) +
  xlab("\nm/z") +
  ylab("H/C\n") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 10, face = "bold"))

p4 <- ggplot(data = VK.plot, aes_string(x = VK.plot$OC, y = VK.plot$HC)) +
  geom_point(shape = 16, aes(size = VK.plot$relInt, colour = VK.plot$composition), alpha = PlotSettings$transparency) +
  geom_vline(xintercept = df.selection$OC) +
  geom_hline(yintercept = df.selection$HC) +
  scale_color_manual(values = PlotSettings$colors) +
  scale_size(range = PlotSettings$bubble_size) +
  scale_y_continuous(limits = c(HCmin, HCmax)) +
  scale_x_continuous(limits = c(OCmin, OCmax)) +
  theme_bw() +
  theme(legend.position = "none", legend.key = element_blank()) +
  xlab("\nO/C") +
  ylab("H/C\n") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 10, face = "bold"))

grid.arrange(p1, p2, p3, p4, ncol = 2)