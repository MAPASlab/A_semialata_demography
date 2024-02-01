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
semialata <- semialata[semialata$Analyses %in% c("niche + demography", "niche"),]
semialata$Longitude <- as.numeric(semialata$Longitude)
semialata$Latitude <- as.numeric(semialata$Latitude)

semi_i <- semialata[semialata$Analyses=="niche + demography" & semialata$Clade=="Clade I",]
semi_ii <- semialata[semialata$Analyses=="niche + demography" & semialata$Clade=="Clade II",]
semi_iii <- semialata[semialata$Analyses=="niche + demography" & semialata$Clade=="Clade IIIa",]
semi_iv <- semialata[semialata$Analyses=="niche + demography" & semialata$Clade=="Clade IV",]

semi_i_n <- semialata[semialata$Analyses=="niche" & semialata$Clade=="Clade I",]
semi_ii_n <- semialata[semialata$Analyses=="niche" & semialata$Clade=="Clade II",]
semi_iii_n <- semialata[semialata$Analyses=="niche" & semialata$Clade=="Clade IIIa",]
semi_iv_n <- semialata[semialata$Analyses=="niche" & semialata$Clade=="Clade IV",]


# Convert data to SpatialPoints and project them
coordinates(semi_i) <- c("Longitude", "Latitude")
proj4string(semi_i) <- CRS("+proj=longlat +datum=WGS84 +type=crs")

coordinates(semi_ii) <- c("Longitude", "Latitude")
proj4string(semi_ii) <- CRS("+proj=longlat +datum=WGS84 +type=crs")

coordinates(semi_iii) <- c("Longitude", "Latitude")
proj4string(semi_iii) <- CRS("+proj=longlat +datum=WGS84 +type=crs")

coordinates(semi_iv) <- c("Longitude", "Latitude")
proj4string(semi_iv) <- CRS("+proj=longlat +datum=WGS84 +type=crs")

coordinates(semi_i_n) <- c("Longitude", "Latitude")
proj4string(semi_i_n) <- CRS("+proj=longlat +datum=WGS84 +type=crs")

coordinates(semi_ii_n) <- c("Longitude", "Latitude")
proj4string(semi_ii_n) <- CRS("+proj=longlat +datum=WGS84 +type=crs")

coordinates(semi_iii_n) <- c("Longitude", "Latitude")
proj4string(semi_iii_n) <- CRS("+proj=longlat +datum=WGS84 +type=crs")

coordinates(semi_iv_n) <- c("Longitude", "Latitude")
proj4string(semi_iv_n) <- CRS("+proj=longlat +datum=WGS84 +type=crs")


# Load world map
data("world", package = "spData")


# Check data distribution to decide area (map window) to be plotted
data("wrld_simpl")
plot(wrld_simpl, line="white", lwd=0.020, col="grey90")

points(semi_i,col="blue3")
points(semi_ii,col="darkgreen")
points(semi_iii,col="goldenrod2")
points(semi_iv,col="red3")

points(semi_i_n,col="blue3")
points(semi_ii_n,col="darkgreen")
points(semi_iii_n,col="goldenrod2")
points(semi_iv_n,col="red3")


# Define map windows
sf_use_s2(F)
win_base <- ext(c(-20, 160, -45, 45))
plot(win_base, border = "gray50", lwd = 0.02, add = TRUE)
my_base <- as.polygons(win_base, crs = "+proj=longlat +datum=WGS84 +type=crs")

mywindow1 <- ext(c(25, 35, -35, -17.5))
plot(mywindow1, border = "gray50", lwd = 0.02, add = TRUE)
my_w1 <- as.polygons(mywindow1, crs="+proj=longlat +datum=WGS84 +type=crs")
mywindow2 <- ext(c(22, 37, -17.5, -1.5))
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
sf_i_n <- st_as_sf(semi_i_n)
sf_ii_n <- st_as_sf(semi_ii_n)
sf_iii_n <- st_as_sf(semi_iii_n)
sf_iv_n <- st_as_sf(semi_iv_n)
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
  geom_sf(data = sf_i_n, color="blue3", pch=2,size=2) +
  geom_sf(data = sf_ii_n, color="darkgreen", pch=0,size=2) +
  geom_sf(data = sf_iii_n, color="goldenrod2",pch=1,size=2) +
  geom_sf(data = sf_iv_n, color="red3",pch=1,size=2) +
  theme_void()
plot(main)


# Save map
ggsave("map_for_niche.pdf", width = 12, height = 6, units = "in", dpi = 300)  

