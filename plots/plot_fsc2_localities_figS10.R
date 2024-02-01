# Load libraries
library(openxlsx)
library(ggplot2)
library(ggpubr)


# Read data files (from Table S5, Model 2c)
# NREL1=N0/N1; NREL2=N1/N2; TCHG1=T1; TCH2=T2
pop_data <- read.xlsx("file.xlsx", sheet = 1)


#Plot data
p1 <- ggplot(data = pop_data, aes(x = CLADE, y = log10(NREL1), color = CLADE)) +
  labs(x = NULL, y = "Ne(T0) / Ne(T1)") +
  geom_boxplot(linewidth = 0.35) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  geom_jitter(size = 0.7, position=position_jitter(0.1)) +
  coord_cartesian(ylim = c(-3, 4.5)) +
  scale_color_manual(values = c("Clade I" = "blue3", "Clade II" = "darkgreen", "Clade IIIa" = "goldenrod2", "Clade IV" = "red3")) +
  theme_light() +
  theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title = element_text(color = "black", family = "sans", face = "plain", size = 11)) +
  theme(axis.text = element_text(color = "black", family = "sans", face = "plain", size = 11)) +
  theme(axis.ticks = element_line(color = "black")) +
  theme(legend.position = "none") +
  theme(aspect.ratio = 1)
p1


p2 <- ggplot(data = pop_data, aes(x = CLADE, y = log10(NREL2), color = CLADE)) +
  labs(x = NULL, y = "Ne(T1) / Ne(T2)") +
  geom_boxplot(linewidth = 0.35) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  geom_jitter(size = 0.7, position=position_jitter(0.1)) +
  coord_cartesian(ylim = c(-3, 4.5))+
  scale_color_manual(values = c("Clade I" = "blue3", "Clade II" = "darkgreen", "Clade IIIa" = "goldenrod2", "Clade IV" = "red3")) +
  theme_light() +
  theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title = element_text(color = "black", family = "sans", face = "plain", size = 11)) +
  theme(axis.text = element_text(color = "black", family = "sans", face = "plain", size = 11)) +
  theme(axis.ticks = element_line(color = "black")) +
  theme(legend.position = "none") +
  theme(aspect.ratio = 1)
p2


p3 <- ggplot(data = pop_data, aes(x = CLADE, y = TCHG1/1000, color = CLADE)) +
  labs(x = NULL, y = "T1 (thousand generations)") +
  geom_boxplot(linewidth = 0.35) +
  geom_jitter(size = 0.7, position=position_jitter(0.1)) +
  coord_cartesian(ylim = c(0.5, 325)) +
  scale_y_log10() +
  scale_color_manual(values = c("Clade I" = "blue3", "Clade II" = "darkgreen", "Clade IIIa" = "goldenrod2", "Clade IV" = "red3")) +
  theme_light() +
  theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title = element_text(color = "black", family = "sans", face = "plain", size = 11)) +
  theme(axis.text = element_text(color = "black", family = "sans", face = "plain", size = 11)) +
  theme(axis.ticks = element_line(color = "black")) +
  theme(legend.position = "none") +
  theme(aspect.ratio = 1)
p3


p4 <- ggplot(data = pop_data, aes(x = CLADE, y = TCHG2/1000, color = CLADE)) +
  labs(x = NULL, y = "T2 (thousand generations)") +
  geom_boxplot(linewidth = 0.35) +
  geom_jitter(size = 0.7, position=position_jitter(0.1)) +
  coord_cartesian(ylim = c(40, 850)) +
  scale_y_log10() +
  scale_color_manual(values = c("Clade I" = "blue3", "Clade II" = "darkgreen", "Clade IIIa" = "goldenrod2", "Clade IV" = "red3")) +
  theme_light() +
  theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title = element_text(color = "black", family = "sans", face = "plain", size = 11)) +
  theme(axis.text = element_text(color = "black", family = "sans", face = "plain", size = 11)) +
  theme(axis.ticks = element_line(color = "black")) +
  theme(legend.position = "none") +
  theme(aspect.ratio = 1)
p4


# Plots for Clade IV only, by region
pop_data <- pop_data[(pop_data$CLADE == "Clade IV"), ]

pop_data[which(pop_data$POP %in% c("AUS2", "AUS3", "AUS4")), c("REGION")] <- c("WA")
pop_data[which(pop_data$POP %in% c("AUS1624", "AUS1625", "AUS1626", "AUS1627", "AUS1628", "AUS1629", "AUS1630", "AUS1631", "AUS1632", "AUS1633", "AUS1634")), c("REGION")] <- c("NT")
pop_data[which(pop_data$POP %in% c("AUS1601", "AUS1602", "AUS1603", "AUS1604", "AUS1605", "AUS1606", "AUS1607", "AUS1608", "AUS1609")), c("REGION")] <- c("FNQ")
pop_data[which(pop_data$POP %in% c("AUS1610", "AUS1611", "AUS1612", "AUS1613", "AUS1614", "AUS1615", "AUS1616", "AUS1617", "AUS1620")), c("REGION")] <- c("SEQ")


p41 <- ggplot(data = pop_data, aes(x = REGION, y = log10(NREL1), colour = CLADE)) +
  labs(x = NULL, y = "Ne(T0) / Ne(T1)") +
  geom_boxplot(linewidth = 0.35) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  geom_jitter(size = 0.7, position=position_jitter(0.1)) +
  coord_cartesian(ylim = c(-3, 4.5))+
  scale_x_discrete(limits=c("WA", "NT", "FNQ", "SEQ")) +
  scale_color_manual(values = c("Clade I" = "blue3", "Clade II" = "darkgreen", "Clade IIIa" = "goldenrod2", "Clade IV" = "red3")) +
  theme_light() +
  theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title = element_text(color = "black", family = "sans", face = "plain", size = 11)) +
  theme(axis.text = element_text(color = "black", family = "sans", face = "plain", size = 11)) +
  theme(axis.ticks = element_line(color = "black")) +
  theme(legend.position = "none") +
  theme(aspect.ratio = 1)
p41 


p42 <- ggplot(data = pop_data, aes(x = REGION, y = log10(NREL2), color = CLADE)) +
  labs(x = NULL, y = "Ne(T1) / Ne(T2)") +
  geom_boxplot(linewidth = 0.35) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  geom_jitter(size = 0.7, position=position_jitter(0.1)) +
  coord_cartesian(ylim = c(-3, 4.5))+
  scale_x_discrete(limits=c("WA", "NT", "FNQ", "SEQ")) +
  scale_color_manual(values = c("Clade I" = "blue3", "Clade II" = "darkgreen", "Clade IIIa" = "goldenrod2", "Clade IV" = "red3")) +
  theme_light() +
  theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title = element_text(color = "black", family = "sans", face = "plain", size = 11)) +
  theme(axis.text = element_text(color = "black", family = "sans", face = "plain", size = 11)) +
  theme(axis.ticks = element_line(color = "black")) +
  theme(legend.position = "none") +
  theme(aspect.ratio = 1)
p42


p43 <- ggplot(data = pop_data, aes(x = REGION, y = TCHG1/1000, color = CLADE)) +
  labs(x = NULL, y = "T1 (thousand generations)") +
  geom_boxplot(linewidth = 0.35) +
  geom_jitter(size = 0.70, position=position_jitter(0.1)) +
  coord_cartesian(ylim = c(0.5, 325))+
  scale_y_log10() +
  scale_x_discrete(limits=c("WA", "NT", "FNQ", "SEQ")) +
  scale_color_manual(values = c("Clade I" = "blue3", "Clade II" = "darkgreen", "Clade IIIa" = "goldenrod2", "Clade IV" = "red3")) +
  theme_light() +
  theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title = element_text(color = "black", family = "sans", face = "plain", size = 11)) +
  theme(axis.text = element_text(color = "black", family = "sans", face = "plain", size = 11)) +
  theme(axis.ticks = element_line(color = "black")) +
  theme(legend.position = "none") +
  theme(aspect.ratio = 1)
p43


p44 <- ggplot(data = pop_data, aes(x = REGION, y = TCHG2/1000, color = CLADE)) +
  labs(x = NULL, y = "T2 (thousand generations)") +
  geom_boxplot(linewidth = 0.35) +
  geom_jitter(size = 0.70, position=position_jitter(0.1)) +
  coord_cartesian(ylim = c(40, 850))+
  scale_y_log10() +
  scale_x_discrete(limits=c("WA", "NT", "FNQ", "SEQ")) +
  scale_color_manual(values = c("Clade I" = "blue3", "Clade II" = "darkgreen", "Clade IIIa" = "goldenrod2", "Clade IV" = "red3")) +
  theme_light() +
  theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title = element_text(color = "black", family = "sans", face = "plain", size = 11)) +
  theme(axis.text = element_text(color = "black", family = "sans", face = "plain", size = 11)) +
  theme(axis.ticks = element_line(color = "black")) +
  theme(legend.position = "none") +
  theme(aspect.ratio = 1)
p44


plots <- ggarrange(p1, p2, p3, p4, p41, p42, p43, p44, ncol = 4, nrow = 2)

ggsave("fsc2_pops.pdf", width = 16, height = 8, units = "in", dpi = 300)

