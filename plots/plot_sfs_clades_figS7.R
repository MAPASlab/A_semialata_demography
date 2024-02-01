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

# Read data (SFS) files
data_files <- list.files(pattern="*.obs$", full.names = FALSE)

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
  
  sfs <- scan(data_files[i], skip = 2)
  sfs_n <- SFSnorm(sfs)
  x = c(1:length(sfs_n))
  x = x/(2*length(sfs_n))
  data <- data.frame(x = x, y = (sfs_n), clade = clade)
  all_data <- rbind(all_data, data)
  
}

# Plot normalized SFS
p <- ggplot(data = all_data, aes(x, y, colour = clade)) +
  geom_line() +
  coord_cartesian(ylim = c(0, 0.7)) +
  scale_x_continuous(breaks=c(0.1, 0.3, 0.5)) +
  labs(x = "Allele frequency", y = "Proportion of sites") +
  scale_colour_manual(values = c("Clade I" = "blue3", "Clade II" = "darkgreen", "Clade IIIa" = "goldenrod2", "Clade IV" = "red3")) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.85)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(family = "sans", face = "plain", size = 14)) +
  theme(axis.title = element_text(family = "sans", face = "plain", size = 14)) +
  theme(axis.text = element_text(family = "sans", face = "plain", size = 14)) +
  theme(aspect.ratio = 1) 
p
  
ggsave("sfs_clades.pdf", width = 5, height = 5, units = "in", dpi = 300)  

