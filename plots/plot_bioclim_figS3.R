# Load libraries
library(dismo)
library(ggplot2)

# Read data (from Bioclim objects obtained for each clade)
bc_i_p <- bioclim_ci@presence
bc_i_p[,3] <- "i"
bc_ii_p <- bioclim_cii@presence
bc_ii_p[,3] <- "ii"
bc_iii_p <- bioclim_ciii@presence
bc_iii_p[,3] <- "iii"
bc_iv_p <- bioclim_civ@presence
bc_iv_p[,3] <- "iv"
bc_db <- rbind(bc_i_p,bc_ii_p,bc_iii_p,bc_iv_p)
names(bc_db) <- c("Temperature", "Precipitation", "Clade")
bc_db$Clade <- as.factor(bc_db$Clade)

# Plot data
p <- ggplot(bc_db, aes(x = Temperature, y = Precipitation, group = Clade)) +
  geom_point(aes(shape = Clade, color = Clade), size = 3.5) +
  coord_cartesian(xlim = c(12.5, 30), ylim = c(500, 3500)) +
  scale_y_continuous(breaks=c(500, 1500, 2500, 3500)) +
  scale_shape_manual(values = c(17, 15, 19, 19)) +
  scale_color_manual(values = c("blue3", "darkgreen", "goldenrod2", "red3")) +
  labs(x = "\nMean Annual Temperature (ºC)", y = "Total Annual Precipitation (mm)\n") +
  theme_light() +
  theme(legend.position = "none") +
  theme(axis.title = element_text(family = "sans", face = "plain", size = 14)) +
  theme(axis.text = element_text(family = "sans", face = "plain", size = 14)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(aspect.ratio = 2/3)
p

ggsave("niche.pdf", width = 9, height = 6, units = "in", dpi = 300)

