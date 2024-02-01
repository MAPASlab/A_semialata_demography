# -----------------------------------------------------------------------
# Project: Sotelo et al., Alloteropsis semialata
# File name: niche_analyses.R
# Last updated: 2023-04-17
# Author: Sara Gamboa
# Email: saragamb@ucm.es
# Repository: https://github.com/MAPASlab/A_semialata_demography
# -----------------------------------------------------------------------
library(dismo)
library(ade4)
library(ecospat)
library(ENMTools)
library(rgdal)
library(terra)
library(sf)
library(landscapemetrics)
library(readxl)
library(ggplot2)

###### 1. We start charging a dataframe that gathers all the information of our samples,
###### including geografical location and clade
semialata <- read_excel("~/Documents/Sara Gamboa/Papers/Graciela/samples_table.xlsx",sheet = 3)
semialata2 <- read_excel("~/Documents/Sara Gamboa/Papers/Graciela/samples_table.xlsx",sheet = 1)

###### 2. We create indivudual datasets for each clade
head(semialata)
semi_i <- semialata[semialata$Clade=="Clade I",]
semi_ii <- semialata[semialata$Clade=="Clade II",]
semi_iii <- semialata[semialata$Clade=="Clade IIIa",]
semi_iv <- semialata[semialata$Clade=="Clade IV",]
semi_iv2 <- semialata2[semialata2$Clade=="Clade IV",]

#### 3. We charge the arrays of current monthly Temperature and Precipitation that we will
#### use to characterize the environmental niche of each clade
setwd("~/Dropbox/Mac/Documents/Sara Gamboa/Papers/Graciela/clima/times")
clim_maps <- list.files()

Prec_0 <- readRDS("Time_0_prec_array.rds")
Temp_0 <- readRDS("Time_0_temp_array.rds")

#### 4. We rasterise the arrays to calculate Mean Temperature and Total Precipitation 
r <- raster(resolution = 0.5, ext = extent(-180, 180, -90, 90))
Temp_stack<- stack()
for (i in 1:12) {
  Temp_0_raster <- raster(x = Temp_0[,,i], template = r)
  Temp_stack <- stack(Temp_stack, Temp_0_raster)
}

Prec_stack<- stack()
for (i in 1:12) {
  Prec_0_raster <- raster(x = Prec_0[,,i], template = r)
  Prec_stack <- stack(Prec_stack, Prec_0_raster)
}

Mean_t <- calc(Temp_stack, fun = mean, na.rm =T)
Prec_A <- calc(Prec_stack, fun = sum, na.rm =T)

plot(Mean_t)
climate_0 <- stack(Mean_t, Prec_A)

#### 5. We convert de data sets into Spatial Points Data Frames
coordinates(semi_i) <- c("Longitude", "Latitude")
coordinates(semi_ii) <- c("Longitude", "Latitude")
coordinates(semi_iii) <- c("Longitude", "Latitude")
coordinates(semi_iv) <- c("Longitude", "Latitude")
coordinates(semi_iv2) <- c("Longitude", "Latitude")

### 6. We perform a Bioclim 'climate-envelope-model'.
bc_i <- bioclim(x=climate_0, p=semi_i)
bc_ii <- bioclim(x=climate_0, p=semi_ii)
bc_iii <- bioclim(x=climate_0, p=semi_iii)
bc_iv <- bioclim(x=climate_0, p=semi_iv)
bc_iv2 <- bioclim(x=climate_0, p=semi_iv2)

### 7. Prepare bioclim data to plot
bc_i_p <- bc_i@presence
bc_i_p[,3] <- "i"
bc_ii_p <- bc_ii@presence
bc_ii_p[,3] <- "ii"
bc_iii_p <- bc_iii@presence
bc_iii_p[,3] <- "iii"
bc_iv_p <- bc_iv@presence
bc_iv_p[,3] <- "iv"
bc_iv2_p <- bc_iv2@presence
bc_iv2_p[,3] <- "iv2"
bc_db <- rbind(bc_i_p,bc_ii_p,bc_iii_p,bc_iv_p,bc_iv2_p)
names(bc_db) <- c("Temperature", "Precipitation", "Clade")
bc_db$Clade <- as.factor(bc_db$Clade)

### 8. Use bioclim climate envelope to predict suitable environments
ext <- c(-180,180,-90,90)
pb_i <- dismo::predict(climate_0, bc_i, ext=ext)
pb_ii <- dismo::predict(climate_0, bc_ii, ext=ext)
pb_iii <- dismo::predict(climate_0, bc_iii, ext=ext)
pb_iv <- dismo::predict(climate_0, bc_iv, ext=ext)
pb_iv2 <- dismo::predict(climate_0, bc_iv2, ext=ext)

pb_i_ab <- pb_i
pb_i_ab@data@values[pb_i_ab@data@values>0] <- 1
pb_ii_ab <- pb_ii
pb_ii_ab@data@values[pb_ii_ab@data@values>0] <- 1
pb_iii_ab <- pb_iii
pb_iii_ab@data@values[pb_iii_ab@data@values>0] <- 1
pb_iv_ab <- pb_iv
pb_iv_ab@data@values[pb_iv_ab@data@values>0] <- 1
pb_iv2_ab <- pb_iv2
pb_iv2_ab@data@values[pb_iv2_ab@data@values>0] <- 1

quartz()
pal_semi <- palette(c("azure3","blue3"))
plot(pb_i_ab, col=pal_semi, legend=F)

pal_semi <- palette(c("azure3","darkgreen"))
plot(pb_ii_ab, col=pal_semi, legend=F)

pal_semi <- palette(c("azure3","goldenrod2"))
plot(pb_iii_ab, col=pal_semi, legend=F)

pal_semi <- palette(c("azure3","red3"))
plot(pb_iv_ab, col=pal_semi, legend=F)
plot(pb_iv2_ab, col=pal_semi, legend=F)


#### 9. We calculate environmental niche overlap using the Ecospat package
### 9.1 data preparaion
climate_0
env <- check.env(climate_0)
points_i <- semialata[semialata$Clade=="Clade I",3:4]
points_ii <- semialata[semialata$Clade=="Clade II",3:4]
points_iii <- semialata[semialata$Clade=="Clade IIIa",3:4]
points_iv <- semialata[semialata$Clade=="Clade IV",3:4]

bg1 = background.points.buffer(points_i, radius = 200000, n = 10*length(semi_i), mask = climate_0[[1]])
bg2 = background.points.buffer(points_ii, radius = 200000, n = 10*length(semi_ii), mask = climate_0[[1]])
bg3 = background.points.buffer(points_iii, radius = 200000, n = 10*length(semi_iii), mask = climate_0[[1]])
bg4 = background.points.buffer(points_iv, radius = 200000, n = 10*length(semi_iv), mask = climate_0[[1]])

occ_bg1 <- bg1
occ_bg2 <- bg2
occ_bg3 <- bg3
occ_bg4 <- bg4
coordinates(occ_bg1) <- c("Longitude", "Latitude")
coordinates(occ_bg2) <- c("Longitude", "Latitude")
coordinates(occ_bg3) <- c("Longitude", "Latitude")
coordinates(occ_bg4) <- c("Longitude", "Latitude")

extract1 = cbind(points_i, extract(climate_0, semi_i), rep(1, nrow(points_i)))
extract2 = cbind(points_ii, extract(climate_0, semi_ii), rep(1, nrow(points_ii)))
extract3 = cbind(points_iii, extract(climate_0, semi_iii), rep(1, nrow(points_iii)))
extract4 = cbind(points_iv, extract(climate_0, semi_iv), rep(1, nrow(points_iv)))

colnames(extract1)[ncol(extract1)] = 'occ'
colnames(extract2)[ncol(extract2)] = 'occ'
colnames(extract3)[ncol(extract3)] = 'occ'
colnames(extract4)[ncol(extract4)] = 'occ'

extbg1 = cbind(bg1, extract(climate_0, occ_bg1), rep(0, nrow(bg1)))
extbg2 = cbind(bg2, extract(climate_0, occ_bg2), rep(0, nrow(bg2)))
extbg3 = cbind(bg3, extract(climate_0, occ_bg3), rep(0, nrow(bg3)))
extbg4 = cbind(bg4, extract(climate_0, occ_bg4), rep(0, nrow(bg4)))

colnames(extbg1)[ncol(extbg1)] = 'occ'
colnames(extbg2)[ncol(extbg2)] = 'occ'
colnames(extbg3)[ncol(extbg3)] = 'occ'
colnames(extbg4)[ncol(extbg4)] = 'occ'

dat1 = rbind(extract1, extbg1)
dat2 = rbind(extract2, extbg2)
dat3 = rbind(extract3, extbg3)
dat4 = rbind(extract4, extbg4)

### 9.2. Calculate overlap, equivalence and similarity
###1 vs 2
pca.env <- dudi.pca(
  rbind(dat1, dat2)[,3:4],
  scannf=FALSE,
  nf=2
)

scores.globclim<-pca.env$li # PCA scores for the whole study area
scores.globclim<-pca.env$li # PCA scores for the whole study area (all points)

scores.sp1 <- suprow(pca.env,
                     extract1[which(extract1[,5]==1),3:4])$li # PCA scores for the species 1 distribution
scores.sp2 <- suprow(pca.env,
                     extract2[which(extract2[,5]==1),3:4])$li # PCA scores for the species 1 distribution

scores.clim1 <- suprow(pca.env,dat1[,3:4])$li # PCA scores for the whole native study area
scores.clim2 <- suprow(pca.env,dat2[,3:4])$li # PCA scores for the whole native study area

grid.clim1 <- ecospat.grid.clim.dyn(
  glob = scores.globclim,
  glob1 = scores.clim1,
  sp = scores.sp1,
  R = 100,
  th.sp = 0
)
grid.clim2 <- ecospat.grid.clim.dyn(
  glob = scores.globclim,
  glob1 = scores.clim2,
  sp = scores.sp2,
  R = 100,
  th.sp = 0
)

quartz.options(width=12/2.54, height=8/2.54)
quartz()
D.overlap <- ecospat.niche.overlap (grid.clim1, grid.clim2, cor=T)$D 
D.overlap

eq.test <- ecospat.niche.equivalency.test(grid.clim1, grid.clim2,
                                          rep=1000, overlap.alternative = "lower") ##rep = 1000 recommended for operational runs
sim.test <- ecospat.niche.similarity.test(grid.clim1, grid.clim2,
                                          rep=1000, overlap.alternative = "lower",
                                          rand.type=2, one.sided=F) 
ecospat.plot.overlap.test(eq.test, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test, "D", "Similarity")

#####1 vs 3
pca.env <- dudi.pca(
  rbind(dat1, dat3)[,3:4],
  scannf=FALSE,
  nf=2
)
scores.globclim<-pca.env$li # PCA scores for the whole study area
scores.globclim<-pca.env$li # PCA scores for the whole study area (all points)

scores.sp1 <- suprow(pca.env,
                     extract1[which(extract1[,5]==1),3:4])$li # PCA scores for the species 1 distribution
scores.sp3 <- suprow(pca.env,
                     extract3[which(extract3[,5]==1),3:4])$li # PCA scores for the species 1 distribution

scores.clim1 <- suprow(pca.env,dat1[,3:4])$li # PCA scores for the whole native study area
scores.clim3 <- suprow(pca.env,dat3[,3:4])$li # PCA scores for the whole native study area

grid.clim1 <- ecospat.grid.clim.dyn(
  glob = scores.globclim,
  glob1 = scores.clim1,
  sp = scores.sp1,
  R = 100,
  th.sp = 0
)
grid.clim3 <- ecospat.grid.clim.dyn(
  glob = scores.globclim,
  glob1 = scores.clim3,
  sp = scores.sp3,
  R = 100,
  th.sp = 0
)

quartz.options(width=12/2.54, height=8/2.54)
quartz()
D.overlap <- ecospat.niche.overlap (grid.clim1, grid.clim3, cor=T)$D 
D.overlap

eq.test <- ecospat.niche.equivalency.test(grid.clim1, grid.clim3,
                                          rep=1000, overlap.alternative = "lower") ##rep = 1000 recommended for operational runs
sim.test <- ecospat.niche.similarity.test(grid.clim1, grid.clim3,
                                          rep=1000, overlap.alternative = "lower",
                                          rand.type=2, one.sided=F) 
ecospat.plot.overlap.test(eq.test, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test, "D", "Similarity")

#####1 vs 4
pca.env <- dudi.pca(
  rbind(dat1, dat4)[,3:4],
  scannf=FALSE,
  nf=2
)
scores.globclim<-pca.env$li # PCA scores for the whole study area
scores.globclim<-pca.env$li # PCA scores for the whole study area (all points)

scores.sp1 <- suprow(pca.env,
                     extract1[which(extract1[,5]==1),3:4])$li # PCA scores for the species 1 distribution
scores.sp4 <- suprow(pca.env,
                     extract4[which(extract4[,5]==1),3:4])$li # PCA scores for the species 1 distribution

scores.clim1 <- suprow(pca.env,dat1[,3:4])$li # PCA scores for the whole native study area
scores.clim4 <- suprow(pca.env,dat4[,3:4])$li # PCA scores for the whole native study area

grid.clim1 <- ecospat.grid.clim.dyn(
  glob = scores.globclim,
  glob1 = scores.clim1,
  sp = scores.sp1,
  R = 100,
  th.sp = 0
)
grid.clim4 <- ecospat.grid.clim.dyn(
  glob = scores.globclim,
  glob1 = scores.clim4,
  sp = scores.sp4,
  R = 100,
  th.sp = 0
)

quartz.options(width=12/2.54, height=8/2.54)
quartz()
D.overlap <- ecospat.niche.overlap (grid.clim1, grid.clim4, cor=T)$D 
D.overlap

eq.test <- ecospat.niche.equivalency.test(grid.clim1, grid.clim4,
                                          rep=1000, overlap.alternative = "lower") ##rep = 1000 recommended for operational runs
sim.test <- ecospat.niche.similarity.test(grid.clim1, grid.clim4,
                                          rep=1000, overlap.alternative = "lower",
                                          rand.type=2, one.sided=F) 
ecospat.plot.overlap.test(eq.test, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test, "D", "Similarity")

#####2 vs 3
pca.env <- dudi.pca(
  rbind(dat2, dat3)[,3:4],
  scannf=FALSE,
  nf=2
)
scores.globclim<-pca.env$li # PCA scores for the whole study area
scores.globclim<-pca.env$li # PCA scores for the whole study area (all points)

scores.sp2 <- suprow(pca.env,
                     extract2[which(extract2[,5]==1),3:4])$li # PCA scores for the species 1 distribution
scores.sp3 <- suprow(pca.env,
                     extract3[which(extract3[,5]==1),3:4])$li # PCA scores for the species 1 distribution

scores.clim2 <- suprow(pca.env,dat2[,3:4])$li # PCA scores for the whole native study area
scores.clim3 <- suprow(pca.env,dat3[,3:4])$li # PCA scores for the whole native study area

grid.clim2 <- ecospat.grid.clim.dyn(
  glob = scores.globclim,
  glob1 = scores.clim2,
  sp = scores.sp2,
  R = 100,
  th.sp = 0
)
grid.clim3 <- ecospat.grid.clim.dyn(
  glob = scores.globclim,
  glob1 = scores.clim3,
  sp = scores.sp3,
  R = 100,
  th.sp = 0
)

quartz.options(width=12/2.54, height=8/2.54)
quartz()
D.overlap <- ecospat.niche.overlap (grid.clim2, grid.clim3, cor=T)$D 
D.overlap

eq.test <- ecospat.niche.equivalency.test(grid.clim2, grid.clim3,
                                          rep=1000, overlap.alternative = "lower") ##rep = 1000 recommended for operational runs
sim.test <- ecospat.niche.similarity.test(grid.clim2, grid.clim3,
                                          rep=1000, overlap.alternative = "lower",
                                          rand.type=2, one.sided=F) 
ecospat.plot.overlap.test(eq.test, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test, "D", "Similarity")

#####2 vs 4
pca.env <- dudi.pca(
  rbind(dat2, dat4)[,3:4],
  scannf=FALSE,
  nf=2
)
scores.globclim<-pca.env$li # PCA scores for the whole study area
scores.globclim<-pca.env$li # PCA scores for the whole study area (all points)

scores.sp2 <- suprow(pca.env,
                     extract2[which(extract2[,5]==1),3:4])$li # PCA scores for the species 1 distribution
scores.sp4 <- suprow(pca.env,
                     extract4[which(extract4[,5]==1),3:4])$li # PCA scores for the species 1 distribution

scores.clim2 <- suprow(pca.env,dat2[,3:4])$li # PCA scores for the whole native study area
scores.clim4 <- suprow(pca.env,dat4[,3:4])$li # PCA scores for the whole native study area

grid.clim2 <- ecospat.grid.clim.dyn(
  glob = scores.globclim,
  glob1 = scores.clim2,
  sp = scores.sp2,
  R = 100,
  th.sp = 0
)
grid.clim4 <- ecospat.grid.clim.dyn(
  glob = scores.globclim,
  glob1 = scores.clim4,
  sp = scores.sp4,
  R = 100,
  th.sp = 0
)
quartz()
D.overlap <- ecospat.niche.overlap (grid.clim2, grid.clim4, cor=T)$D 
D.overlap

eq.test <- ecospat.niche.equivalency.test(grid.clim2, grid.clim4,
                                          rep=1000, overlap.alternative = "lower") ##rep = 1000 recommended for operational runs
sim.test <- ecospat.niche.similarity.test(grid.clim2, grid.clim4,
                                          rep=1000, overlap.alternative = "lower",
                                          rand.type=2, one.sided=F) 
ecospat.plot.overlap.test(eq.test, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test, "D", "Similarity")

#####3 vs 4
pca.env <- dudi.pca(
  rbind(dat3, dat4)[,3:4],
  scannf=FALSE,
  nf=2
)
scores.globclim<-pca.env$li # PCA scores for the whole study area
scores.globclim<-pca.env$li # PCA scores for the whole study area (all points)

scores.sp3 <- suprow(pca.env,
                     extract3[which(extract3[,5]==1),3:4])$li # PCA scores for the species 1 distribution
scores.sp4 <- suprow(pca.env,
                     extract4[which(extract4[,5]==1),3:4])$li # PCA scores for the species 1 distribution

scores.clim3 <- suprow(pca.env,dat3[,3:4])$li # PCA scores for the whole native study area
scores.clim4 <- suprow(pca.env,dat4[,3:4])$li # PCA scores for the whole native study area

grid.clim3 <- ecospat.grid.clim.dyn(
  glob = scores.globclim,
  glob1 = scores.clim3,
  sp = scores.sp3,
  R = 100,
  th.sp = 0
)
grid.clim4 <- ecospat.grid.clim.dyn(
  glob = scores.globclim,
  glob1 = scores.clim4,
  sp = scores.sp4,
  R = 100,
  th.sp = 0
)
quartz()
D.overlap <- ecospat.niche.overlap (grid.clim3, grid.clim4, cor=T)$D 
D.overlap

eq.test <- ecospat.niche.equivalency.test(grid.clim3, grid.clim4,
                                          rep=1000, overlap.alternative = "lower") ##rep = 1000 recommended for operational runs
sim.test <- ecospat.niche.similarity.test(grid.clim3, grid.clim4,
                                          rep=1000, overlap.alternative = "lower",
                                          rand.type=2, one.sided=F) 
ecospat.plot.overlap.test(eq.test, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test, "D", "Similarity")

### 10. To explore the past environmental niche availability for  each clade and it relation with present distribution
### we use the PALEO-PGEM climatic layers. In order to use the we reproject them to an equal area projection
###and predict past available environmental space.
setwd("~/Dropbox/Mac/Documents/Sara Gamboa/Papers/Graciela/clima/times")

pb_i_l <- list()
pb_ii_l <- list()
pb_iii_l <- list()
pb_iv_l <- list()
pb_iv2_l <- list()

p <- 1
t <- 2
r <- raster(resolution = 0.5, ext = extent(-180, 180, -90, 90))
EqualEarthProj= "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

for (i in 1:5001) {
  PREC <- readRDS(clim_maps[p])
  TEMP <- readRDS(clim_maps[t])
  
  Temp_stack<- stack()
  for (j in 1:12) {
    Temp_raster <- raster(x = TEMP[,,j], template = r)
    Temp_raster <- projectRaster(Temp_raster, crs = EqualEarthProj)
    Temp_stack <- stack(Temp_stack, Temp_raster)
  }
  
  Prec_stack<- stack()
  for (k in 1:12) {
    Prec_raster <- raster(x = PREC[,,k], template = r)
    Prec_raster <- projectRaster(Prec_raster, crs = EqualEarthProj)
    Prec_stack <- stack(Prec_stack, Prec_raster)
  }
  
  Mean_t <- calc(Temp_stack, fun = mean, na.rm =T)
  Prec_A <- calc(Prec_stack, fun = sum, na.rm =T)
  
  climate_temp <- stack(Mean_t, Prec_A)
  
  pb_i_temp <- dismo::predict(climate_temp, bc_i)
  pb_i_l[i] <- pb_i_temp
  # pb_ii_temp <- dismo::predict(climate_temp, bc_ii)
  # pb_ii_l[i] <- pb_ii_temp
  # pb_iii_temp<- dismo::predict(climate_temp, bc_iii)
  # pb_iii_l[i] <- pb_iii_temp
  # pb_iv_temp <- dismo::predict(climate_temp, bc_iv)
  # pb_iv_l[i] <- pb_iv_temp
  # pb_iv2_temp <- dismo::predict(climate_temp, bc_iv2)
  # pb_iv2_l[i] <- pb_iv2_temp
  
  p <- p+2
  t <- t+2
  
  print(i)
}


pb_i_s <- stack(pb_i_l)
pb_ii_s <- stack(pb_ii_l)
pb_iii_s <- stack(pb_iii_l)
pb_iv_s <- stack(pb_iv_l)
pb_iv2_s <- stack(pb_iv2_l)

### 11. We load an prepare shapefiles to mask continents
setwd("~/Dropbox/Mac/Documents/Sara Gamboa/Papers/Fragmentation/Reclassified_data/Continents_masks")
Africa <- readOGR("Africa.shp")
Eurasia <- readOGR("Eurasia_mod.shp")
Africa_r <- spTransform(Africa,crs(pb_i))
Eurasia_r <- spTransform(Eurasia,crs(pb_i))

areatime <- as.data.frame(matrix(ncol=9,nrow=5001))
colnames(areatime) <- c("time", "clade_i", "clade_ii", "clade_iii", "clade_iva", "clade_ib","clade_iib","clade_iiib","clade_ivb")
areatime$time <- c(1:5001)

Africa_p <- pb_i[[1]]
values(Africa_p) <- 1
result <- Africa_p %>% mask(Africa_r)

pb_i_masked <- pb_i*result
pb_i_masked[pb_i_masked>0] <- 1
pb_i_masked[pb_i_masked==0] <- NA
pb_ii_masked <- pb_ii*result
pb_ii_masked[pb_ii_masked>0] <- 1
pb_ii_masked[pb_ii_masked==0] <- NA
pb_iii_masked <- pb_iii*result
pb_iii_masked[pb_iii_masked>0] <- 1
pb_iii_masked[pb_iii_masked==0] <- NA
pb_iva_masked <- pb_iv*result
pb_iva_masked[pb_iva_masked>0] <- 1
pb_iva_masked[pb_iva_masked==0] <- NA
Eurasia_p <- pb_i[[1]]
values(Eurasia_p) <- 1
result <- Eurasia_p %>% mask(Eurasia_r)

pb_ib_masked <- pb_i*result
pb_ib_masked[pb_ib_masked>0] <- 1
pb_ib_masked[pb_ib_masked==0] <- NA

pb_iib_masked <- pb_ii*result
pb_iib_masked[pb_iib_masked>0] <- 1
pb_iib_masked[pb_iib_masked==0] <- NA

pb_iiib_masked <- pb_iii*result
pb_iiib_masked[pb_iiib_masked>0] <- 1
pb_iiib_masked[pb_iiib_masked==0] <- NA

pb_ivb_masked <- pb_iv*result
pb_ivb_masked[pb_ivb_masked>0] <- 1
pb_ivb_masked[pb_ivb_masked==0] <- NA

###We calculate the potential area available for every clade in Africa and Eurasia-Oceania
for (i in 1:5001) {
  area <- lsm_p_area(pb_i_masked[[i]], directions = 8)
  areatime$clade_i[i] <- sum(area$value)
}
for (i in 1:5001) {
  area <- lsm_p_area(pb_ib_masked[[i]], directions = 8)
  areatime$clade_ib[i] <- sum(area$value)
}
for (i in 1:5001) {
  area <- lsm_p_area(pb_ii_masked[[i]], directions = 8)
  areatime$clade_ii[i] <-  sum(area$value)
}
for (i in 1:5001) {
  area <- lsm_p_area(pb_iib_masked[[i]], directions = 8)
  areatime$clade_iib[i] <-  sum(area$value)
}
for (i in 1:5001) {
  area <- lsm_p_area(pb_iii_masked[[i]], directions = 8)
  areatime$clade_iii[i] <-  sum(area$value)
}
for (i in 1:5001) {
  area <- lsm_p_area(pb_iiib_masked[[i]], directions = 8)
  areatime$clade_iiib[i] <-  sum(area$value)
}
for (i in 1:5001) {
  area <- lsm_p_area(pb_iva_masked[[i]], directions = 8)
  areatime$clade_iva[i] <-  sum(area$value)
}
for (i in 1:5001) {
  area <- lsm_p_area(pb_ivb_masked[[i]], directions = 8)
  areatime$clade_ivb[i] <-  sum(area$value)
}


plot(pb_iv_aus[[i]])
i <- 2
for (i in 1:5001) {
  area <- lsm_p_area(pb_iv_aus[[i]], directions = 8)
  areaafrica$clade_iv_aus[i] <-  sum(area$value)
}

names(areatime)
areatime[10] <- areatime[2] + areatime[6]
areatime[11] <- areatime[3] + areatime[7]
areatime[12] <- areatime[4] + areatime[8]
areatime[13] <- areatime[5] + areatime[9]
areatotal <- areatime[,c(1,10:13)]
head(areaafrica)
areaafrica <- areatime[,c(1:5)]

names(areatotal) <- c("time", "clade_i","clade_ii","clade_iii","clade_iv")
names(areaafrica) <- c("time", "clade_i","clade_ii","clade_iii","clade_iv")

library(writexl)
write_xlsx(areatime2,"tabla_areas_2.xlsx")
write_xlsx(areaafrica,"tabla_areas_africa.xlsx")

#########Stability
plot(pb_i[[2]])
crs.mol <- CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
pb_i_mol <- projectRaster(pb_i, crs=crs.mol)
pb_ii_mol <- projectRaster(pb_ii, crs=crs.mol)
pb_iii_mol <- projectRaster(pb_iii, crs=crs.mol)
pb_iv_mol <- projectRaster(pb_iv, crs=crs.mol)

pb_i_s[pb_i_s>0] <- 1
pb_ii_s[pb_ii_s>0] <- 1
pb_iii_s[pb_iii_s>0] <- 1
pb_iv_s[pb_iv_s>0] <- 1
pb_iv2_s[pb_iv2_s>0] <- 1

est_pb_i <- pb_i_s[[1]]
for (i in 2:501) {
  est_pb_i <- est_pb_i * pb_i_s[[i]]
  print(i)
}
est_pb_ii <- pb_ii_s[[1]]
for (i in 2:501) {
  est_pb_ii <- est_pb_ii * pb_ii_s[[i]]
  print(i)
}
est_pb_iii <- pb_iii_s[[1]]
for (i in 2:501) {
  est_pb_iii <- est_pb_iii * pb_iii_s[[i]]
  print(i)
}
est_pb_iv <- pb_iv_s[[1]]
for (i in 2:501) {
  est_pb_iv <- est_pb_iv * pb_iv_s[[i]]
  print(i)
}
est_pb_iv2 <- pb_iv2_s[[1]]
for (i in 2:501) {
  est_pb_iv2 <- est_pb_iv2 * pb_iv2_s[[i]]
  print(i)
}

quartz()
plot(est_pb_i)

sum_est_i_5 <-cellStats(est_pb_i, 'sum')
sum_est_ii_5 <-cellStats(est_pb_ii, 'sum')
sum_est_iii_5 <-cellStats(est_pb_iii, 'sum')
sum_est_iv_5 <-cellStats(est_pb_iv, 'sum')
sum_est_i_25 <-cellStats(est_pb_i, 'sum')
sum_est_ii_25 <-cellStats(est_pb_ii, 'sum')
sum_est_iii_25 <-cellStats(est_pb_iii, 'sum')
sum_est_iv_25 <-cellStats(est_pb_iv, 'sum')
sum_est_i_20 <-cellStats(est_pb_i, 'sum')
sum_est_ii_20 <-cellStats(est_pb_ii, 'sum')
sum_est_iii_20 <-cellStats(est_pb_iii, 'sum')
sum_est_iv_20 <-cellStats(est_pb_iv, 'sum')
sum_est_i_15 <-cellStats(est_pb_i, 'sum')
sum_est_ii_15 <-cellStats(est_pb_ii, 'sum')
sum_est_iii_15 <-cellStats(est_pb_iii, 'sum')
sum_est_iv_15 <-cellStats(est_pb_iv, 'sum')
sum_est_i_10 <-cellStats(est_pb_i, 'sum')
sum_est_ii_10 <-cellStats(est_pb_ii, 'sum')
sum_est_iii_10 <-cellStats(est_pb_iii, 'sum')
sum_est_iv_10 <-cellStats(est_pb_iv, 'sum')
sum_est_i_05 <-cellStats(est_pb_i, 'sum')
sum_est_ii_05 <-cellStats(est_pb_ii, 'sum')
sum_est_iii_05 <-cellStats(est_pb_iii, 'sum')
sum_est_iv_05 <-cellStats(est_pb_iv, 'sum')

estability <- cbind(c(sum_est_i_5,sum_est_ii_5,sum_est_iii_5,sum_est_iv_5),
                    c(sum_est_i_25,sum_est_ii_25,sum_est_iii_25,sum_est_iv_25),
                    c(sum_est_i_20,sum_est_ii_20,sum_est_iii_20,sum_est_iv_20),
                    c(sum_est_i_15,sum_est_ii_15,sum_est_iii_15,sum_est_iv_15),
                    c(sum_est_i_10,sum_est_ii_10,sum_est_iii_10,sum_est_iv_10),
                    c(sum_est_i_05,sum_est_ii_05,sum_est_iii_05,sum_est_iv_05))
colnames(estability) <- c("5Ma", "2.5Ma", "2Ma","1.5Ma","1Ma", "0.5Ma")
rownames(estability) <- c("Clade I","Clade II","Clade III","Clade IV")

saveRDS(pb_i,"pb_i.rds")
saveRDS(pb_ii,"pb_ii.rds")
saveRDS(pb_iii,"pb_iii.rds")
saveRDS(pb_iv,"pb_iv.rds")


# est_pb_ivb <- pb_iv[[1]]
# for (i in 2:501) {
#   est_pb_ivb <- est_pb_ivb * pb_iv[[i]]
#   print(i)
# }
# sum_est_ivb_05 <-cellStats(est_pb_ivb, 'sum')
# est_pb_ivb <- pb_iv[[1]]
# for (i in 2:1001) {
#   est_pb_ivb <- est_pb_ivb * pb_iv[[i]]
#   print(i)
# }
# sum_est_ivb_10 <-cellStats(est_pb_ivb, 'sum')
# est_pb_ivb <- pb_iv[[1]]
# for (i in 2:1501) {
#   est_pb_ivb <- est_pb_ivb * pb_iv[[i]]
#   print(i)
# }
# sum_est_ivb_15 <-cellStats(est_pb_ivb, 'sum')
# est_pb_ivb <- pb_iv[[1]]
# for (i in 2:2001) {
#   est_pb_ivb <- est_pb_ivb * pb_iv[[i]]
#   print(i)
# }
# sum_est_ivb_20 <-cellStats(est_pb_ivb, 'sum')
# est_pb_ivb <- pb_iv[[1]]
# for (i in 2:2501) {
#   est_pb_ivb <- est_pb_ivb * pb_iv[[i]]
#   print(i)
# }
# sum_est_ivb_25 <-cellStats(est_pb_ivb, 'sum')
# est_pb_ivb <- pb_iv[[1]]
# for (i in 2:5001) {
#   est_pb_ivb <- est_pb_ivb * pb_iv[[i]]
#   print(i)
# }
# sum_est_ivb_50 <-cellStats(est_pb_ivb, 'sum')

quartz()
plot(est_pb_iv)
crs_mp <- "+proj=longlat +datum=WGS84 +no_defs"
# pb_i_na<-crop(pb_i_na, mywindow)
# pb_ii_na<-crop(pb_ii_na, mywindow)
# pb_iii_na<-crop(pb_iii_na, mywindow)
# pb_iv_na<-crop(pb_iv_na, mywindow)

est_pb_i <- pb_i_s[[1]]
for (i in 2:2501) {
  est_pb_i <- est_pb_i * pb_i_s[[i]]
  print(i)
}
est_pb_ii <- pb_ii_s[[1]]
for (i in 2:2501) {
  est_pb_ii <- est_pb_ii * pb_ii_s[[i]]
  print(i)
}
est_pb_iii <- pb_iii_s[[1]]
for (i in 2:5001) {
  est_pb_iii <- est_pb_iii * pb_iii_s[[i]]
  print(i)
}
est_pb_iv <- pb_iv_s[[1]]
for (i in 2:1001) {
  est_pb_iv <- est_pb_iv * pb_iv_s[[i]]
  print(i)
}
est_pb_iv2 <- pb_iv2_s[[1]]
for (i in 2:501) {
  est_pb_iv2 <- est_pb_iv2 * pb_iv2_s[[i]]
  print(i)
}

plot(est_pb_i)
est_i <- projectRaster(est_pb_i, crs=crs_mp)
est_ii <- projectRaster(est_pb_ii, crs=crs_mp)
est_iii <- projectRaster(est_pb_iii, crs=crs_mp)
est_iv <- projectRaster(est_pb_iv, crs=crs_mp)
est_iv2 <- projectRaster(est_pb_iv2, crs=crs_mp)


quartz()
pal_semi <- palette(c("azure3","blue3"))
plot(est_i, col=pal_semi, legend=F, main="Estability for clade I in the last 2.5 Ma")

pal_semi <- palette(c("azure3","darkgreen"))
plot(est_ii, col=pal_semi, legend=F,main="Estability for clade II in the last 2.5 Ma")

pal_semi <- palette(c("azure3","azure3"))
plot(est_iii, col=pal_semi, legend=F,main="Estability for clade III")

pal_semi <- palette(c("azure3","red3"))
plot(est_iv, col=pal_semi, legend=F,main="Estability for clade IV in the last 5 Ma")
plot(est_iv2, col=pal_semi, legend=F,main="Estability for clade IV in the last 0.5 Ma")

sum_est_i_20 <-cellStats(est_pb_i, 'sum')
sum_est_ii_20 <-cellStats(est_pb_ii, 'sum')
sum_est_iii_20 <-cellStats(est_pb_iii, 'sum')
sum_est_iv_20 <-cellStats(est_pb_iv, 'sum')

plot(pb_i_na[[1]])

plot(mywindow, border = "red", lwd = 2, add = TRUE)
pb_i_na<-crop(pb_i_na, mywindow)
plot(pb_iv_na[[1]])
plot(pb_iv[[1]])


sum(values(pb_i_na[[1]]), na.rm=TRUE)


library(sf)

plot(est_pb_iv)

