# Generate base map with samples, further edited with inkscape (https://inkscape.org/)

# Load libraries
library(maptools)
library(terra)
library(spData)
library(sf)
library(ggplot2)
library(openxlsx)

# Read and filter data (from Table S1)
semialata <- read.xlsx("samples_table.xlsx", sheet = 1)
semialata <- semialata[semialata$Analyses=="niche + demography",]
semialata$Longitude <- as.numeric(semialata$Longitude)
semialata$Latitude <- as.numeric(semialata$Latitude)

semi_i <- semialata[semialata$Clade=="Clade I",]
semi_ii <- semialata[semialata$Clade=="Clade II",]
semi_iii <- semialata[semialata$Clade=="Clade IIIa",]
semi_iv <- semialata[semialata$Clade=="Clade IV",]


# Convert data to SpatialPoints and project them
coordinates(semi_i) <- c("Longitude", "Latitude")
proj4string(semi_i) <- CRS("+proj=longlat +datum=WGS84 +type=crs")

coordinates(semi_ii) <- c("Longitude", "Latitude")
proj4string(semi_ii) <- CRS("+proj=longlat +datum=WGS84 +type=crs")

coordinates(semi_iii) <- c("Longitude", "Latitude")
proj4string(semi_iii) <- CRS("+proj=longlat +datum=WGS84 +type=crs")

coordinates(semi_iv) <- c("Longitude", "Latitude")
proj4string(semi_iv) <- CRS("+proj=longlat +datum=WGS84 +type=crs")


# Load world map
data("world", package = "spData")

# Check data distribution to decide area (map window) to be plotted
data("wrld_simpl")
plot(wrld_simpl, line="white", lwd=0.020, col="grey90")

points(semi_i,col="blue3")
points(semi_ii,col="darkgreen")
points(semi_iii,col="goldenrod2")
points(semi_iv,col="red3")


# Define map windows
sf_use_s2(F)
win_base <- ext(c(-20, 160, -45, 45))
plot(win_base, border = "gray50", lwd = 0.02, add = TRUE)
my_base <- as.polygons(win_base, crs = "+proj=longlat +datum=WGS84 +type=crs")

mywindow1 <- ext(c(25, 35, -35, -17.5))
plot(mywindow1, border = "gray50", lwd = 0.02, add = TRUE)
my_w1 <- as.polygons(mywindow1, crs="+proj=longlat +datum=WGS84 +type=crs")
mywindow2 <- ext(c(22, 37, -15, -4.5))
plot(mywindow2, border = "gray50", lwd = 0.02, add = TRUE)
my_w2 <- as.polygons(mywindow2, crs="+proj=longlat +datum=WGS84 +type=crs")
mywindow3 <- ext(c(125, 155, -30, -10))
plot(mywindow3, border = "gray50", lwd = 0.02, add = TRUE)
my_w3 <- as.polygons(mywindow3, crs="+proj=longlat +datum=WGS84 +type=crs")

base <- st_crop(world, my_base)
cropped1<-st_crop(world, my_w1)
cropped2<-st_crop(world, my_w2)
cropped3<-st_crop(world, my_w3)


# Convert objects to sf format
sf_i <- st_as_sf(semi_i)
sf_ii <- st_as_sf(semi_ii)
sf_iii <- st_as_sf(semi_iii)
sf_iv <- st_as_sf(semi_iv)
sf_w1 <- st_as_sf(my_w1)
sf_w2 <- st_as_sf(my_w2)
sf_w3 <- st_as_sf(my_w3)
mainbox1 <- st_bbox(my_w1)
mainbox2 <- st_bbox(my_w2)
mainbox3 <- st_bbox(my_w3)


# Plot map with sampled sites
sf_use_s2(T)
main <- ggplot() + 
  geom_sf(data = base, fill="grey90") +
  geom_sf(data = sf_i, fill="blue3",color="black", pch=24,size=2) +
  geom_sf(data = sf_ii, fill="darkgreen",color="black", pch=22,size=2) +
  geom_sf(data = sf_iii, fill="goldenrod2",color="black",pch=21,size=2) +
  geom_sf(data = sf_iv, fill="red3",color="black",pch=21,size=2) +
  theme_void()
plot(main)

inset <- ggplot() + 
  geom_sf(data = cropped2, fill = "grey90") +
  geom_sf(data = sf_ii, fill="darkgreen",color = "black",size=5, pch=22) +
  geom_sf(data = sf_iii, fill="goldenrod2",color = "black",size=5, pch=21) +
  geom_sf(data = sf_w2, fill = NA) +
  theme_void() +
  theme(legend.position = "none")
plot(inset)


# Save maps
ggsave("mapmain.pdf", plot = main, width = 12, height = 6, units = "in", dpi = 300)
ggsave("mapinset.pdf", plot = inset, width = 12, height = 6, units = "in", dpi = 300)

