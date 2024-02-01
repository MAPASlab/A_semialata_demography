# Load libraries
library(ggplot2)

# Read data (from Stairway Plot 2 output files)
data_files <- list.files(pattern="*_ind30.final.summary", full.names=FALSE)

clade1 <- 1
clade2 <- 2
clade3 <- 3
clade4 <- 4

all_data <- data.frame()

for (i in 1:length(data_files)){

  if (i %in% clade1) {
    clade <- "Clade I"
  } else if (i %in% clade2) {
    clade <- "Clade II"
  } else if (i %in% clade3) {
    clade <- "Clade IIIa"
  } else if (i %in% clade4){
    clade <- "Clade IV"
  }
  
  data <- read.table(data_files[i], header = TRUE)
  data$clade <- clade
  
  all_data <- rbind(all_data, data)
  
}

# Plot data
p <- ggplot(data = all_data, aes(x = year/5, y = Ne_median/1000, colour = clade)) +
  geom_line(linewidth = 0.8) +
  geom_ribbon(aes(ymin = Ne_2.5./1000, ymax = Ne_97.5./1000, fill = clade), linetype = "blank", alpha = 0.10) +
  geom_vline(xintercept = c(50000, 150000, 300000), linewidth = 0.5, color = "black", linetype = "dashed") +
  labs(x = "Generations ago", y = "Ne (thousands)") +
  coord_cartesian(xlim = c(50, 450000), ylim = c(10, 5000)) +
  scale_y_log10() +
  scale_colour_manual(values = c("Clade I" = "blue3", "Clade II" = "darkgreen", "Clade IIIa" = "goldenrod2", "Clade IV" = "red3")) +
  scale_fill_manual(values = c("Clade I" = "blue3", "Clade II" = "darkgreen", "Clade IIIa" = "goldenrod2", "Clade IV" = "red3")) +
  theme_classic() +
  theme(axis.title = element_text(family = "sans", face = "plain", size = 14)) +
  theme(axis.text = element_text(family = "sans", face = "plain", size = 12)) +
  theme(legend.text = element_text(family = "sans", face = "plain", size = 14)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(0.875, 0.85)) +
  theme(aspect.ratio = 2/3) 
p

ggsave("stair_clades.pdf", width = 7.5, height = 5, units = "in", dpi = 300)

