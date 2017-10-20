df.selection <- VK.plot[row_selected,]


p1 <- ggplot(data = VK.plot, aes_string(x = VK.plot$mz, y = VK.plot$`error (ppm)`)) +
  geom_point(shape = 3, size = 1) +
  geom_vline(xintercept = df.selection$mz) +
  geom_hline(yintercept = df.selection$`error (ppm)`) +
  scale_x_continuous(limits = c(mzmin, mzmax)) +
  scale_y_continuous(limits = c(errormin, errormax)) +
  theme_bw() +
  theme(legend.position = "bottom", legend.key = element_blank()) +
  xlab("\nm/z") +
  ylab("Error (ppm)\n") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 10, face = "bold"))

p2 <- ggplot(data = VK.plot, aes_string(x = VK.plot$mz, y = VK.plot$AMD)) +
  geom_point(shape = 3, size = 1) +
  geom_vline(xintercept = df.selection$mz) +
  geom_hline(yintercept = df.selection$AMD) +
  scale_x_continuous(limits = c(mzmin, mzmax)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  theme(legend.position = "bottom", legend.key = element_blank()) +
  xlab("\nm/z") +
  ylab("AMD\n") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 10, face = "bold"))

grid.arrange(p1, p2, ncol = 2)

