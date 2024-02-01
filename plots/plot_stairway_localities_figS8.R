# Load libraries
library(ggplot2)

# Plot all localities separately and arrange them in a single figure with faceting 
# to get 80 final localities, for a square facet plot, include JMS file three times (copies)
# and remove copies from final pdf using inkscape (https://inkscape.org/)

# Read data (from Stairway Plot 2 output files)
data_files <- list.files(pattern="*.final.summary$", full.names=FALSE)

filenames <- gsub("_filt4.final.summary", "", data_files, fixed = TRUE)
filenames <- gsub("clade_i_", "", filenames, fixed = TRUE)
filenames <- gsub("clade_ii_", "", filenames, fixed = TRUE)
filenames <- gsub("clade_iiia_", "", filenames, fixed = TRUE)
filenames <- gsub("clade_iv_", "", filenames, fixed = TRUE)
filenames

clade1 <- 1:16
clade2 <- 17:33
clade3 <- 34:48
clade4 <- 49:80

data <- data.frame()

for (i in 1:length(data_files)){

  pop <- read.table(data_files[i], header = TRUE)
  pop$pop <- filenames[i]

  if (i %in% clade1) {
    pop$clade <- "Clade I"
  } else if (i %in% clade2) {
    pop$clade <- "Clade II"
  } else if (i %in% clade3) {
    pop$clade <- "Clade IIIa"
  } else if (i %in% clade4){
    pop$clade <- "Clade IV"
  }

  data <- rbind(data, pop)
  
}

# Order localities for plotting
levels1 <- c("ZIM1502","ZIM1503","ZIM1504","SFB","LSU","ASM","MTP","CRL","MDB","EML","SNR","BLW","KWT","JMS","JMS1","JMS2")
levels2 <- c("ZAM1715","ZAM1503","ZAM1716","ZAM1507","JKO12","JKO13","JKO23","ZAM1723","ZAM1936","TAN2","TAN1602","TAN1604","ZAM1939","ZAM1961","ZAM1941","ZAM1940","TAN1")
levels3 <- c("TAN1603a","ZAM1505a","ZAM1711","JKO8","JKO9","JKO6","JKO5","JKO4","JKO2","ZAM1706","ZAM1707","ZAM1708","ZAM1712","ZAM1726","ZAM1701")
levels4 <- c("AUS2","AUS3","AUS4","AUS1628","AUS1624","AUS1633","AUS1631","AUS1634","AUS1627","AUS1625","AUS1630","AUS1629","AUS1626","AUS1632","AUS1609","AUS1608","AUS1607","AUS1606","AUS1605","AUS1604","AUS1603", "AUS1602","AUS1601","AUS1620","AUS1617","AUS1616","AUS1615","AUS1614","AUS1613","AUS1612","AUS1611","AUS1610")
all_levels <- c(levels1, levels2, levels3, levels4)
data$pop <- factor(data$pop, levels = all_levels)

# Plot data
p <- ggplot(data = data, aes(x = year / 5, y = Ne_median / 1000, colour = clade)) +
  geom_line(linewidth = 0.25) +
  geom_ribbon(aes(ymin = Ne_2.5./1000, ymax = Ne_97.5./1000, fill = clade), linetype = "blank", alpha = 0.05) +
  labs(x = "Generations ago", y = "Ne (thousands)") +
  coord_cartesian(xlim = c(50, 450000), ylim = c(1, 1000)) +
  scale_x_continuous(breaks = c(100000, 300000)) +
  scale_y_log10(breaks=c(10, 1000)) +
  scale_colour_manual(values = c("Clade I" = "blue3", "Clade II" = "darkgreen", "Clade IIIa" = "goldenrod2", "Clade IV" = "red3")) +
  scale_fill_manual(values = c("Clade I" = "blue3", "Clade II" = "darkgreen", "Clade IIIa" = "goldenrod2", "Clade IV" = "red3")) +
  theme_light() +
  theme(panel.grid.major = element_line(linewidth = 0.25), panel.grid.minor = element_blank()) +
  theme(legend.position = "none", strip.background = element_blank()) +
  theme(strip.text = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.title = element_text(family = "sans", face = "plain", size = 20)) +
  theme(axis.text = element_text(family = "sans", face = "plain", size = 16)) +
  theme(aspect.ratio = 1/2) +
  facet_wrap(~pop, ncol = 8)
p

ggsave("stair_pops_facet.pdf", width = 20, height = 15, units = "in", dpi = 300)

