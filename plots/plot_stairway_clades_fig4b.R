# Load libraries
library(ggplot2)
library(ggpubr)

# Read and organize data (from Stairway Plot 2 output files)
data_files <- list.files(pattern="*.final.summary$", full.names=FALSE)

filenames <- gsub("_filt4.final.summary", "", data_files, fixed = TRUE)
filenames <- gsub("clade_i_", "", filenames, fixed = TRUE)
filenames <- gsub("clade_ii_", "", filenames, fixed = TRUE)
filenames <- gsub("clade_iiia_", "", filenames, fixed = TRUE)
filenames <- gsub("clade_iv_", "", filenames, fixed = TRUE)
filenames

clade1 <- 1:14
clade2 <- 15:31
clade3 <- 32:46
clade4 <- 47:78

all_data <- data.frame()

for (i in 1:length(data_files)){  

  if (i %in% clade1) {
    data$clade <- "Clade I" 
  } else if (i %in% clade2) {
    data$clade <- "Clade II" 
  } else if (i %in% clade3) {
    data$clade <- "Clade IIIa" 
  } else if (i %in% clade4) {
    data$clade <- "Clade IV" 
  }

  pop <- read.table(data_files[i], header = TRUE)
  pop$pop <- filenames[i]
  pop$clade <- clade
  all_data <- rbind(all_data, pop)
  
}

data_files1 = data_files[1:14]
data_files2 = data_files[15:31]
data_files3 = data_files[32:46]
data_files4 = data_files[47:78]

filenames1 = filenames[1:14]
filenames2 = filenames[15:31]
filenames3 = filenames[32:46]
filenames4 = filenames[47:78]

pop1=data.frame()
pop2=data.frame()
pop3=data.frame()
pop4=data.frame()

data1=data.frame()
data2=data.frame()
data3=data.frame()
data4=data.frame()


for (i in 1:length(data_files1)){
	
	pop1 <- read.table(data_files1[i], header = TRUE)
	pop1$pop <- filenames1[i]
	pop1$clade <- "Clade I"
	data1 <- rbind(data1, pop1)

}


for (i in 1:length(data_files2)){
	
	pop2 <- read.table(data_files2[i], header = TRUE)
	pop2$pop <- filenames2[i]
	pop2$clade <- "Clade II"
	data2 <- rbind(data2, pop2)

}


for (i in 1:length(data_files3)){
	
	pop3 <- read.table(data_files3[i], header = TRUE)
	pop3$pop <- filenames3[i]
	pop3$clade <- "Clade IIIa"
	data3 <- rbind(data3, pop3)

}


for (i in 1:length(data_files4)){
	
	pop4 <- read.table(data_files4[i], header = TRUE)
	pop4$pop <- filenames4[i]
	pop4$clade <- "Clade IV"
	data4 <- rbind(data4,pop4)

}


data1$grouping <- NA
data2$grouping <- NA
data3$grouping <- NA
data4$grouping <- NA

data1$grouping <- c("Group_1")
data1_1 <- data1[(data1$grouping == "Group_1"),]

data2[which(!data2$pop %in% c("TAN2", "ZAM1716")), c("grouping")] <- c("Group_1")
data2[which(data2$pop %in% c("TAN2", "ZAM1716")), c("grouping")] <- c("Group_2")
data2_1 <- data2[(data2$grouping == "Group_1"),]

data3[which(!data3$pop %in% c("TAN1603a", "ZAM1505a")), c("grouping")] <- c("Group_1")
data3[which(data3$pop %in% c("TAN1603a", "ZAM1505a")), c("grouping")] <- c("Group_2")
data3_1 <- data3[(data3$grouping == "Group_1"),]

data4[which(data4$pop %in% c( "AUS1624", "AUS1625", "AUS1626", "AUS1627", "AUS1628", "AUS1629", "AUS1630", "AUS1631", "AUS1632", "AUS1633", "AUS1634")), c("grouping")] <- c("1-NT")
data4[which(data4$pop %in% c("AUS2", "AUS3", "AUS4")), c("grouping")] <- c("2-WA")
data4[which(data4$pop %in% c("AUS1601", "AUS1602", "AUS1603", "AUS1604", "AUS1605", "AUS1606", "AUS1607", "AUS1608", "AUS1609")), c("grouping")] <- c("3-FNQ")
data4[which(data4$pop %in% c("AUS1610", "AUS1611", "AUS1612", "AUS1613", "AUS1614", "AUS1615", "AUS1616", "AUS1617", "AUS1620")), c("grouping")] <- c("4-SEQ")
data4_1 <- data4[(data4$grouping == "1-NT"),]
data4_2 <- data4[(data4$grouping == "2-WA"),]
data4_3 <- data4[(data4$grouping == "3-FNQ"),]
data4_4 <- data4[(data4$grouping == "4-SEQ"),]


# Plot data
p1 <- ggplot(data = data1_1, aes(x = year / 5, y = Ne_median / 1000, group = pop)) +  
  geom_line(linewidth = 0.6, color = "blue3") +
  labs(x = NULL, y = NULL) +
  coord_cartesian(xlim = c(50, 450000), ylim = c(1, 1000)) +
  scale_y_log10() +
  theme_classic() +
  theme(axis.text = element_text(family = "sans", face = "plain", size = 14)) +
  theme(legend.position = "none") +
  theme(aspect.ratio = 2/3)
p1

p2 <- ggplot(data = data2_1, aes(x = year / 5, y = Ne_median / 1000, group = pop)) +  
  geom_line(linewidth = 0.6, color = "darkgreen") +
  labs(x = NULL, y = NULL) +
  coord_cartesian(xlim = c(50, 450000), ylim = c(1, 1000)) +
  scale_y_log10() +
  theme_classic() +
  theme(axis.text = element_text(family = "sans", face = "plain", size = 14)) +
  theme(legend.position = "none") +
  theme(aspect.ratio = 2/3)
p2

p3 <- ggplot(data = data3_1, aes(x = year / 5, y = Ne_median / 1000, group = pop)) +  
  geom_line(linewidth = 0.6, color = "goldenrod2") +
  labs(x = NULL, y = NULL) +
  coord_cartesian(xlim = c(50, 450000), ylim = c(1, 1000)) +
  scale_y_log10() +
  theme_classic() +
  theme(axis.text = element_text(family = "sans", face = "plain", size = 14)) +
  theme(legend.position = "none") +
  theme(aspect.ratio = 2/3)
p3


p4_1 <- ggplot(data = data4_1, aes(x = year / 5, y = Ne_median / 1000, group = pop)) +  
  geom_line(linewidth = 0.6, color = "red3") +
  labs(x = NULL, y = NULL) +
  coord_cartesian(xlim = c(50, 450000), ylim = c(1, 1000)) +
  scale_y_log10() +
  theme_classic() +
  theme(axis.text = element_text(family = "sans", face = "plain", size = 14)) +
  theme(legend.position = "none") +
  theme(aspect.ratio = 2/3)
p4_1

p4_2 <- ggplot(data = data4_2, aes(x = year / 5, y = Ne_median / 1000, group = pop)) +  
  geom_line(linewidth = 0.6, color = "red3") +
  labs(x = NULL, y = NULL) +
  coord_cartesian(xlim = c(50, 450000), ylim = c(1, 1000)) +
  scale_y_log10() +
  theme_classic() +
  theme(axis.text = element_text(family = "sans", face = "plain", size = 14)) +
  theme(legend.position = "none") +
  theme(aspect.ratio = 2/3)
p4_2

p4_3 <- ggplot(data = data4_3, aes(x = year / 5, y = Ne_median / 1000, group = pop)) +  
  geom_line(linewidth = 0.6, color = "red3") +
  labs(x = NULL, y = NULL) +
  coord_cartesian(xlim = c(50, 450000), ylim = c(1, 1000)) +
  scale_y_log10() +
  theme_classic() +
  theme(axis.text = element_text(family = "sans", face = "plain", size = 14)) +
  theme(legend.position = "none") +
  theme(aspect.ratio = 2/3)
p4_3

p4_4 <- ggplot(data = data4_4, aes(x = year / 5, y = Ne_median / 1000, group = pop)) +  
  geom_line(linewidth = 0.6, color = "red3") +
  labs(x = NULL, y = NULL) +
  coord_cartesian(xlim = c(50, 450000), ylim = c(1, 1000)) +
  scale_y_log10() +
  theme_classic() +
  theme(axis.text = element_text(family = "sans", face = "plain", size = 14)) +
  theme(legend.position = "none") +
  theme(aspect.ratio = 2/3)
p4_4


plot_1st <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)
ggsave("upper_plot.pdf.pdf", width = 12, height = 4, units = "in", dpi = 300)

plot_2nd <- ggarrange(p4_1, p4_2, p4_3, p4_4, ncol = 4, nrow = 1)
ggsave("lower_plot.pdf.pdf", width = 16, height = 4, units = "in", dpi = 300)

