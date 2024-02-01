# Load libraries
library(ggplot2)


# Normalize SFS (function by Paolo Momigliano)
SFSnorm <- function(SFS) {
  SFS<-SFS[2:((length(SFS)-1)/2+1)]
  SFS<-as.data.frame(SFS)
  colnames(SFS)[1]<-"s"
  SFS$i<-c(1:length(SFS$s))
  SFS$f<-SFS$i / (2*length(SFS$s))
  SFS$fs<-SFS$s/sum(SFS$s)
  SFS$norm<-"NA"
  
  for(i in 1:length(SFS$f)){
    n<-length(SFS$f)
    SFS$norm[i]<- SFS$fs[i]*(i*(2*n-i)/(2*n))
  }
  return(as.numeric(unlist(SFS$norm)))
}


# Read and summarize SFS info
data_files=list.files(pattern="*.sfs$", full.names=FALSE)
summary <- data.frame(file = data_files, ind = NA, monomorphic = NA, bialleic = NA)
count <- 0

for (i in 1:length(data_files)){
  
  count <- count + 1
  clade_pop <- gsub("_filt4.sfs", "", data_files[i], fixed=TRUE)
  sfs <- scan(data_files[i])
  n_ind <- (length(sfs)- 1)/2
  mono <- sfs[1]
  bi <- sum(sfs[2:length(sfs)])
  summary[i,2:4] <- c(n_ind, mono, bi)
  
}

write.table(summary, file = "SFS_pops.txt", quote = FALSE, sep = " ")


# Plot SFS from all localities and arrange them with faceting
# to get 80 final localities, for a square facet plot, include JMS file three times (copies)
# and remove copies from final pdf using inkscape (https://inkscape.org/)
# read data files
data_files <- list.files(pattern="*.sfs$", full.names = FALSE)

clade1 <- 1:16
clade2 <- 17:33
clade3 <- 34:48
clade4 <- 49:80

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
  
  pop <- gsub("_filt4.sfs", "", data_files[i], fixed = TRUE)
  pop <- gsub("^\\d+", "", pop, perl = TRUE)
  pop <- gsub("-", "", pop, fixed = TRUE)
  
  sfs <- scan(data_files[i])
  sfs_n <- SFSnorm(sfs)
  x = c(1:length(sfs_n))
  x = x/(2*length(sfs_n))
  data <- data.frame(x = x, y = (sfs_n), pop = pop, clade = clade)
  all_data <- rbind(all_data, data)
  
}

# order localities for plotting
levels1 <- c("ZIM1502","ZIM1503","ZIM1504","SFB","LSU","ASM","MTP","CRL","MDB","EML","SNR","BLW","KWT","JMS","JMS1","JMS2")
levels2 <- c("ZAM1715","ZAM1503","ZAM1716","ZAM1507","JKO12","JKO13","JKO23","ZAM1723","ZAM1936","TAN2","TAN1602","TAN1604","ZAM1939","ZAM1961","ZAM1941","ZAM1940","TAN1")
levels3 <- c("TAN1603a","ZAM1505a","ZAM1711","JKO8","JKO9","JKO6","JKO5","JKO4","JKO2","ZAM1706","ZAM1707","ZAM1708","ZAM1712","ZAM1726","ZAM1701")
levels4 <- c("AUS2","AUS3","AUS4","AUS1628","AUS1624","AUS1633","AUS1631","AUS1634","AUS1627","AUS1625","AUS1630","AUS1629","AUS1626","AUS1632","AUS1609","AUS1608","AUS1607","AUS1606","AUS1605","AUS1604","AUS1603", "AUS1602","AUS1601","AUS1620","AUS1617","AUS1616","AUS1615","AUS1614","AUS1613","AUS1612","AUS1611","AUS1610")
all_levels <- c(levels1, levels2, levels3, levels4)
all_data$pop <- factor(all_data$pop, levels = all_levels)

# plot
p <- ggplot(data = all_data, aes(x, y, colour = clade)) +
  geom_line(linewidth = 0.5) +
  labs(x = "\nAllele frequency", y = "Proportion of sites\n") +
  coord_cartesian(ylim = c(0, 0.7)) +
  scale_y_continuous(breaks=c(0.1, 0.3, 0.5)) +
  scale_x_continuous(breaks=c(0.5)) +
  scale_colour_manual(values = c("Clade I" = "blue3", "Clade II" = "darkgreen", "Clade IIIa" = "goldenrod2", "Clade IV" = "red3")) +  scale_fill_manual(values = c("Clade I" = "blue3", "Clade II" = "darkgreen", "Clade IIIa" = "goldenrod2", "Clade IV" = "red3")) +
  theme_light() +
  theme(panel.grid.major.y = element_line(linewidth = 0.25), panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none", strip.background = element_blank()) +
  theme(strip.text = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.title = element_text(family = "sans", face = "plain", size = 20)) +
  theme(axis.text = element_text(family = "sans", face = "plain", size = 16)) +
  theme(aspect.ratio = 1) +
  facet_wrap(~pop, ncol = 8)
p

ggsave("sfs_pops_facets.pdf", width = 15, height = 20, units = "in", dpi = 300)

