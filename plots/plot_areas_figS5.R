# Load libraries
library(openxlsx)
library(ggplot2)
library(ggpubr)

# Read area data (from Table S3)
data <- read.xlsx("areas.xlsx", sheet = 1)

# Build a single dataframe
data_tot <- data.frame(time=(data[,1]), area=c(data[,2], data[,3], data[,4], data[,5]), clade=c(rep("Clade I", 5001), rep("Clade II", 5001), rep("Clade IIIa", 5001), rep("Clade IV", 5001)), region = "1-Total")
data_afr <- data.frame(time=(data[,1]), area=c(data[,6], data[,7], data[,8], data[,9]), clade=c(rep("Clade I", 5001), rep("Clade II", 5001), rep("Clade IIIa", 5001), rep("Clade IV", 5001)), region = "2-Africa")
data_aus <- data.frame(time=(data[,1]), area=(data[,10]), clade=(rep("Clade IV", 5001)), region = "3-Australia")

# Plot data (panels of Figure S5, further arranged using inkscape https://inkscape.org/)
# panels on Figure S5a
p1 <- ggplot(data = data_tot, aes(x = time/1000000, y = area/1000000, group = interaction(region,clade), color = clade)) +
  geom_line(linewidth = 0.5) +
  #labs(x = "\nTime (million years ago)", y = "Suitable area (million km2)\n") +
  labs(x = NULL, y = NULL) +
  coord_cartesian(xlim = c(0,5), ylim = c(0,32.50)) +
  scale_color_manual(values = c("Clade I" = "blue3", "Clade II" = "darkgreen", "Clade IIIa" = "goldenrod2", "Clade IV" = "red3")) +
  theme_classic() +
  theme(panel.grid.major = element_line(linewidth = 0.25), panel.grid.minor = element_blank()) +
  theme(legend.position = "none", strip.background = element_blank()) +
  theme(strip.text = element_text(family = "sans", face = "plain", size = 12, color = "black")) +
  theme(legend.title = element_blank()) +
  theme(axis.title = element_text(size = 18)) +
  theme(axis.text = element_text(size = 18)) +
  theme(aspect.ratio = 1/4) 
p1

p2 <- ggplot(data = data_afr, aes(x = time/1000000, y = area/1000000, group = interaction(region,clade), color = clade)) +
  geom_line(linewidth = 0.5) +
  #labs(x = "\nTime (million years ago)", y = "Suitable area (million km2)\n") +
  labs(x = NULL, y = NULL) +
  coord_cartesian(xlim = c(0,5), ylim = c(0,15)) +
  scale_color_manual(values = c("Clade I" = "blue3", "Clade II" = "darkgreen", "Clade IIIa" = "goldenrod2", "Clade IV" = "red3")) +
  theme_classic() +
  theme(panel.grid.major = element_line(linewidth = 0.25), panel.grid.minor = element_blank()) +
  theme(legend.position = "none", strip.background = element_blank()) +
  theme(strip.text = element_text(family = "sans", face = "plain", size = 12, color = "black")) +
  theme(legend.title = element_blank()) +
  theme(axis.title = element_text(size = 18)) +
  theme(axis.text = element_text(size = 18)) +
  theme(aspect.ratio = 1/4) 
p2

p3 <- ggplot(data = data_aus, aes(x = time/1000000, y = area/1000000, group = interaction(region,clade), color = clade)) +
  geom_line(linewidth = 0.5) +
  #labs(x = "\nTime (million years ago)", y = "Suitable area (million km2)\n") +
  labs(x = NULL, y = NULL) +
  coord_cartesian(xlim = c(0,5), ylim = c(0,5)) +
  scale_color_manual(values = c("Clade I" = "blue3", "Clade II" = "darkgreen", "Clade IIIa" = "goldenrod2", "Clade IV" = "red3")) +
  theme_classic() +
  theme(panel.grid.major = element_line(linewidth = 0.25), panel.grid.minor = element_blank()) +
  theme(legend.position = "none", strip.background = element_blank()) +
  theme(strip.text = element_text(family = "sans", face = "plain", size = 12, color = "black")) +
  theme(legend.title = element_blank()) +
  theme(axis.title = element_text(size = 18)) +
  theme(axis.text = element_text(size = 18)) +
  theme(aspect.ratio = 1/4) 
p3

# panels on Figure S5b
data_r <- data_afr[data_afr$clade == "Clade I", ]

p4 <- ggplot(data = data_r, aes(x = time/1000000, y = area/1000000, group = interaction(region,clade), color = clade)) +
  geom_line(linewidth = 0.5) +
  #labs(x = "\nTime (million years ago)", y = "Suitable area (million km2)\n") +
  labs(x = NULL, y = NULL) +
  coord_cartesian(xlim = c(0,5), ylim = c(0,5)) +
  scale_color_manual(values = c("Clade I" = "blue3", "Clade II" = "darkgreen", "Clade IIIa" = "goldenrod2", "Clade IV" = "red3")) +
  theme_classic() +
  theme(panel.grid.major = element_line(linewidth = 0.25), panel.grid.minor = element_blank()) +
  theme(legend.position = "none", strip.background = element_blank()) +
  theme(strip.text = element_text(family = "sans", face = "plain", size = 12, color = "black")) +
  theme(legend.title = element_blank()) +
  theme(axis.title = element_text(size = 18)) +
  theme(axis.text = element_text(size = 18)) +
  theme(aspect.ratio = 1/4) 
p4

data_r <- data_afr[(data_afr$clade %in% c("Clade II", "Clade IIIa")), ]

p5 <- ggplot(data = data_r, aes(x = time/1000000, y = area/1000000, group = interaction(region,clade), color = clade)) +
  geom_line(linewidth = 0.5) +
  #labs(x = "\nTime (million years ago)", y = "Suitable area (million km2)\n") +
  labs(x = NULL, y = NULL) +
  coord_cartesian(xlim = c(0,5), ylim = c(0,5)) +
  scale_color_manual(values = c("Clade I" = "blue3", "Clade II" = "darkgreen", "Clade IIIa" = "goldenrod2", "Clade IV" = "red3")) +
  theme_classic() +
  theme(panel.grid.major = element_line(linewidth = 0.25), panel.grid.minor = element_blank()) +
  theme(legend.position = "none", strip.background = element_blank()) +
  theme(strip.text = element_text(family = "sans", face = "plain", size = 12, color = "black")) +
  theme(legend.title = element_blank()) +
  theme(axis.title = element_text(size = 18)) +
  theme(axis.text = element_text(size = 18)) +
  theme(aspect.ratio = 1/4) 
p5

data_r <- data_afr[data_afr$clade == "Clade IV", ]

p6 <- ggplot(data = data_r, aes(x = time/1000000, y = area/1000000, group = interaction(region,clade), color = clade)) +
  geom_line(linewidth = 0.5) +
  #labs(x = "\nTime (million years ago)", y = "Suitable area (million km2)\n") +
  labs(x = NULL, y = NULL) +
  coord_cartesian(xlim = c(0,5), ylim = c(10,15)) +
  scale_color_manual(values = c("Clade I" = "blue3", "Clade II" = "darkgreen", "Clade IIIa" = "goldenrod2", "Clade IV" = "red3")) +
  theme_classic() +
  theme(panel.grid.major = element_line(linewidth = 0.25), panel.grid.minor = element_blank()) +
  theme(legend.position = "none", strip.background = element_blank()) +
  theme(strip.text = element_text(family = "sans", face = "plain", size = 12, color = "black")) +
  theme(legend.title = element_blank()) +
  theme(axis.title = element_text(size = 18)) +
  theme(axis.text = element_text(size = 18)) +
  theme(aspect.ratio = 1/4) 
p6

plots <- ggarrange(p1,p2,p3,p4,p5,p6, ncol = 1, nrow = 6)

ggsave("plots_area.pdf", height = 24, width = 16, units = "in", dpi = 300)

