#############################################################################
# Set of functions used to read genotype matrices ##
# Vitor Sousa 06/07/2021
#############################################################################

####################################################
# READ FILES 
####################################################

# READ_GT_FILE
# reads the a file with the genotypes of individuals for each SNP
# the file is a matrix with nsnps rows and nind columns
# INPUT:
#   filename : string with the name of the file
#   nindgtfile : number of individuals coded in GT file
# OUTPUT:
#   matrix with nind x nspns with the genotypes
read_gt_file <- function(filename, nindgtfile) {
  gendata <- matrix(scan(paste("./data/codedGT",filename,sep="")), byrow=T, ncol=nindgtfile)
  # replace the missing data char by NA (in this case the missing data char was set to 9)
  gendata[gendata==9] <- NA
  # get matrix with nind x nsnp
  t(gendata)  
}

# READ_GQ_FILE
# reads the a file with the genotype quality for each genotype
# the file is a matrix with nsnps rows and nind columns
# INPUT:
#   filename : string with the name of the file
#   nindgtfile : number of individuals coded in GQ file
# OUTPUT:
#   matrix with nind x nspns with the genotype quality
read_gq_file <- function(filename, nindgtfile) {
  gq <- matrix(scan(paste("./data/GQ",filename,sep="")), byrow=T, ncol=nindgtfile)
  # replace the missing data char by NA (in this case the missing data char was set to 0)
  gq[gq==0] <- NA
  # get matrix with nind x nsnp
  t(gq)  
}


# CHECK_GQ
# check that the GQ filter worked correctly
# INPUT
#   gq : genotype quality nind x nspns matrix
#   gq_threshold : theshold used for filtering
#   tgendata : called genotypes nind x nspns matrix
# RETURN
#   tgendata : called genotypes nind x nspns matrix, where values less than threshold are replaced by NA
check_gq <- function(gq, gq_threshold, tgendata) {
  if(sum(gq<gq_threshold, na.rm=T)>0) {
    print(paste("ERROR: there are genotypes called with quality less than threshold", gq_threshold, ". All these replaced by NA."))
    # Replace tgendata with less than Genotype quality 9 by 'NA'
    tgendata[gq<9] <- NA
  }
  # Plot Genotype Quality
  hist(gq, xlim=c(0,100), breaks=c(0:100,1000), main=paste(filename, "Genotype Quality"), xlab="GQ", prob=T)
  dev.print(device=pdf, width=6, height=4, file=paste(filename,"_GQ_zoom.pdf",sep=""))
  hist(gq, xlim=c(0,100), main=paste(filename, "Genotype Quality"), xlab="GQ", prob=T)
  dev.print(device=pdf, width=6, height=4, file=paste(filename,"_GQ.pdf",sep=""))  
  tgendata
}

# READ_DP_FILE
# reads the a file with the coverage depth DP for each genotype
# the file is a matrix with nsnps rows and nind columns
# INPUT:
#   filename : string with the name of the file
#   nindgtfile : number of individuals coded in DP file
# OUTPUT:
#   matrix with nind x nspns with the coverage depth DP
read_dp_file <- function(filename, nindgtfile) {
  dp <- matrix(scan(paste("./data/DP",filename,sep="")), byrow=T, ncol=nindgtfile)
  # get matrix with nind x nsnp
  t(dp)  
}

# PRINT_DP_HIST
# prints the distribution of the depth coverage DP
# INPUT
#   dp : nind x nsnps matrix with the DP for each ind and each site
#   filename : string with filename
# RETURN
#   prints file paste(filename,"_DP.pdf",sep="")
print_dp_hist <- function(dp, filename) {
  hist(dp, xlim=c(0,40), breaks=c(0:40,10000), main=paste(filename, "Coverage Depth"), xlab="DP", prob=T)
  legend("topright", paste("mean(dp>0)=",format(mean(dp[dp>0]),digits=4)))
  dev.print(device=pdf, width=6, height=4, file=paste(filename,"_DP.pdf",sep=""))  
}


# READ_IND_POP_INFO
# reads a file with the IDs, sampling location and distance for each individual
# INPUT:
#   filename : string with the name of the file
# OUTPUT:
#   data frame with nind x 4 columns with  indID, popID, OnOff, distance
read_ind_pop_info <- function(filename, phenotype) {
  indpopinfo <- read.table(paste(filename,sep=""), colClasses=c("character","character","numeric","numeric"), header=T) 
  indpopinfo
}


# READ_IND_POP_PHENOTYPE_INFO
# reads a file with the IDs, sampling location, distance, sex and phenotypic info for each individual 
# INPUT:
#   filename : string with the name of the file
#   numphenotypes : number of numeric columns after ID, SAMPLE, Dist(NtoS), Dist(StoN), sex columns of file filename
# OUTPUT:
#   data frame with nind x 4 columns with  indID, popID, distance, distance
read_ind_pop_phenotype_info <- function(filename, numphenotypes) {
  indpopinfo <- read.table(paste(filename,sep=""), colClasses=c("character","character","numeric","numeric","numeric","character",rep.int("character", numphenotypes)), header=T) 
  indpopinfo
}


# READ_IND_ID_VCF
# read the individual ID labels in the VCF file
# INPUT:
#   filename : string with the name of the file
# OUTPUT:
#   data frame with nind x 3 columns with  indID, N_SITES, MEAN_DEPTH
read_ind_id_vcf <- function(filename) {
  indidvcf <- read.table(paste("./data/",filename,".idepth",sep=""), colClasses=c("character", "numeric", "numeric"), header=T)  
  indidvcf
}


# GET_INDPOPINFO_INDVCF
# get index of individuals for which we have pop info
# this is for cases where the VCF file contains a different number of individuals tha the pop info
# we only keep the individuals that have the same ID in the ind pop info and in the VCF file.
# INPUT:
#   indpopinfo : data frame with ind and pop info information - only requirement first column must have individual ID
#   indidvcf   : data frame with ind ID labels in VCF file - only requirement first column must have individual ID
# OUTPUT:
#   list with the $indexinpopinfo and $indexretainvcf
#   with the index of individuals in both files for the indpopinfo and VCF file respectively.
#   It prints the number of individuals in common.
get_indpopinfo_indvcf <- function(indpopinfo, indidvcf) {
  count <- 0
  indexinpopinfo <- numeric() # index of the indid individuals in indpopinfo
  indexretainvcf <- numeric() # index of individuals to retain from indid 
  for(i in 1:length(indidvcf[,1])) {
    if (sum(indpopinfo[,1]==indidvcf[i,1]) > 0) {
      indexinpopinfo <- c(indexinpopinfo, which(indpopinfo[,1]==indidvcf[i,1]))
      indexretainvcf <- c(indexretainvcf, i)
      count <- count + 1 
    }
  }
  print(count)
  
  # Check that the order of individuals is the same
  if(sum(indpopinfo[indexinpopinfo,1]!=indidvcf[indexretainvcf,1])>0) {
    stop("Error!! In function get_indpopinfo_indvcf: indpopinfo ind id is not the same as indidvcf in the vcf file")    
  }
  
  list(indexinpopinfo=indexinpopinfo,indexretainvcf=indexretainvcf)  
}


# SORT_IND_DISTANCE
# sort individuals according to distance from the northern COLOME sample
# INPUT:
#   indpopinfo : data frame with nind x 4 columns with  indID, popID, OnOff, distance
#   indidvcf   : data frame with nind x 3 columns with  indID, N_SITES, MEAN_DEPTH
#   indexretainvcf : index of individuals in common for indidvcf
#   columndist : column in indpopinfo with the distance
# OUTPUT:
#   list with sorted.indpopinfo (sorted index in the indpopinfo) and 
#   sorted.indexretaininvecf (sorted index in vcf)
sort_ind_distance <- function(vcf.indpopinfo, indidvcf, indexretainvcf, columndist) {
  
  # sort individuals according distance to COLOME sampling location
  # NOTE: all individuals sampled in a given location considered to have the same distance
  index.sort.dist <- order(vcf.indpopinfo[,columndist])
  indexretainvcf <- indexretainvcf[index.sort.dist]

  if(sum(vcf.indpopinfo[index.sort.dist,1]!=indidvcf[indexretainvcf,1])>0){
    stop("ERROR!! Function sort_ind_distance: the vcf.sortedindpopinfo and indidvcf individuals labels do not have the same order!")
  }
    
  list(sorted.indpopinfo=index.sort.dist, sorted.indexretainvcf=indexretainvcf)  
}


####################################################
# DATA QUALITY 
####################################################


# PLOT_SNP_CONTIG_DISTRIBUTION
# plot the distribution of the length of contigs and the number of SNPs per contig
# INPUT:
#   sites : data.frame with four columns corresponding to CHROM, POS, MEAN_DEPTH, VAR_DEPTH
# OUTPUT:
#   save file "distribution_snps_per_contig.pdf"
plot_snp_contig_distribution <- function(sites) {  
  Cairo(width=10, height=7, units="in", type="pdf", file="distribution_snps_per_contig.pdf") 
  par(mfrow=c(1,1))
  plot(table(sites[,1]), main="with all loci", xlab="scaffolds", ylab="frequency")
  mtext(paste("snps=",sum(table(sites[,1])),", mean snps/contig=",  format(mean(table(sites[,1])), digits=4)), side=3)   
  dev.off() 
}



# PLOT_LENGTH_CONTIG_DISTRIBUTION
# plots the distribution of the length of the scaffolds
# INPUT:
#   sites : data.frame with four columns corresponding to CHROM, POS, MEAN_DEPTH, VAR_DEPTH
#   contigs : vector with length ncontigs with the labels of the contigs
# OUTPUT:
#   save file "distribution_contig_size.pdf"
plot_length_contig_distribution <- function(sites, contigs) {
  contigsize <- numeric(length(contigs))
  for(i in 1:length(contigs)) {
    evaluate <- sites[,1]==contigs[i]
    contigsize[i] <- max(as.numeric(sites[evaluate,2]))-min(as.numeric(sites[evaluate,2]))
  }
  Cairo(width=10, height=7, units="in", type="pdf", file="distribution_contig_size.pdf") 
  hist(contigsize, main="Distribution of scaffold length", xlab="scaffold length", prob=T)
  legend("topright",c(paste("contigs=",sum(contigsize>0)),paste("contigs(<1kb)=",sum(contigsize<1000)),paste("mean=",mean(contigsize)),paste("median=",median(contigsize))))
  plot(contigsize, ylim=c(0,1000), main="Length (<1000) of scaffolds", xlab="scaffold index", ylab="scaffold length")
  dev.off()
}


# PLOT_MEAN_COVERAGE_PER_SITE
# plot distribution of mean coverage per site
# INPUT:
#   sites : data.frame with four columns corresponding to CHROM, POS, MEAN_DEPTH, VAR_DEPTH
# OUTPUT:
#   save file "mean_coverage_per_site.pdf" and "mean_vs_sd_coverage_per_site.pdf"
plot_mean_coverage_per_site <- function(sites) {
  par(mfrow=c(1,2))
  Cairo(width=6, height=6, file="mean_coverage_per_site.pdf", units="in", type="pdf") 
  hist(sites[,3], main="mean coverage per site", xlab="mean coverage per site\n(discarding missing data)", prob=T, xlim=c(0,200))
  legend("topright",c(paste("mean=",mean(sites[,3])),paste("min=",min(sites[,3])),paste("max=",max(sites[,3]))))
  hist(sites[,4], main="variance coverage per site", xlab="variance coverage per site\n(discarding missing data)", prob=T, xlim=c(0,5000))
  dev.off()  
  
  par(mfrow=c(1,1))
  Cairo(width=6, height=6, file="mean_vs_sd_coverage_per_site.pdf", units="in", type="pdf") 
  plot(sites[,3], sqrt(sites[,4]), xlab="mean coverage per site", ylab="stand. dev. coverage per site", pch=".")
  dev.off()  
}


# PLOT_FIS
# plots the distribution of the mean FIS per individual for each population
# INPUT:
#   filename : header of VCF filename
#   vcf.sortedindpopinfo : data.frame with the ind and pop info (nind rows x 4 columns with indID, popID, OnOff, distance)
#   commonind : list with the index of individuals in info pop and vcf files
# OUTPUT:
#   plot saved in file "Fis_perpop.pdf"
#   numeric array with the FIS per individual sorted according to distance from the north
plot_fis <- function(filename, vcf.sortedindpopinfo, commonind) {
  # read data
  fis_idvcf <- read.table(paste(filename,".het",sep=""), colClasses=c("character", "numeric", "numeric","numeric", "numeric"), header=T)
  pop.names <- unique(vcf.sortedindpopinfo[,2])
  # plot
  Cairo(width=10, height=7, file="Fit_perpop.pdf", units="in", type="pdf") 
  par(mfrow=c(3,5))
  for(i in 1:length(pop.names)) {
    eval <- vcf.sortedindpopinfo[,2]==pop.names[i]
    # sort these individuals according to distance from north
    hist(fis_idvcf[commonind$sorted.indexretainvcf[eval],5], main=paste("FIT", pop.names[i]), xlab="FIT", prob=T, xlim=c(-1,1))
  }
  dev.off()  
  fis_idvcf[commonind$sorted.indexretainvcf,5]
}


# PLOT_MISSINGDATA_IND
# plots distribution of mean missing data per individual
# INPUT
#   filename : header of VCF filename
#   vcf.sortedindpopinfo : data.frame with the ind and pop info (nind rows x 4 columns with indID, popID, OnOff, distance)
#   commonind : list with the index of individuals in info pop and vcf files
#   northsouth : vector of size nind with code for sampling location of individuals
# OUTPUT:
#   output saved in file "missingdata_per_ind.pdf"
#   numeric array with the mean missing data per individual sorted according to distance from the north
plot_missingdata_ind <- function(filename, vcf.sortedindpopinfo, commonind, northsouth) {
  indmissing <- read.table(paste(filename,".imiss",sep=""), colClasses=c("character", "numeric", "numeric", "numeric", "numeric"), header=T)
  # sort individuals according to vcf.sortedindpopinfo
  sorted.indmissing <- indmissing[commonind$sorted.indexretainvcf,]
  # check that individuals are sorted
  if(sum(vcf.sortedindpopinfo[,1]!=sorted.indmissing[,1])>0){
    stop("ERROR!! Function plot_missingdata_ind: vcf.sortedindpopinfo and indidvcf individuals labels do not have the same order.")
  }
  
  # Distribution of missing data per individual
  # bars colored according to population
  Cairo(width=10, height=7, file="missingdata_per_ind.pdf", units="in", type="pdf") 
  par(mfrow=c(1,1))
  barcolors <- as.numeric(as.factor(vcf.sortedindpopinfo[,2]))
  spacing <- c(rep.int(0.2, times=max(which(northsouth==1))), 20, rep.int(0.2, times=-1+max(which(northsouth==2))-max(which(northsouth==1))), 20, rep.int(0.2, times=-1+max(which(northsouth==3))-max(which(northsouth==2))))
  barplot(sorted.indmissing[,5], col=barcolors, border=barcolors,  ylim=c(0,1), space=spacing)
  legend("topright", unique(vcf.sortedindpopinfo[,2]), fill=as.numeric(as.factor(unique(vcf.sortedindpopinfo[,2]))), cex=0.7)
  mtext(side=3, text=paste("mean missing data per ind.=",mean(sorted.indmissing[,5]), "\nmedian missing data per ind.=",median(sorted.indmissing[,5])))
  dev.off()
  sorted.indmissing[,5]
}


# PLOT_FREQALLELE_GENFREQ_SAMPLE
# plot the distribution of genotype frequency as a function of the allele frequency in the sample
# INPUT
#   tgendata   : called genotypes nspns x ninds matrix
#   indpopinfo : data frame with nind x 4 columns with  indID, popID, OnOff, distance (vcf.sortedindpopinfo[,1:4])
# OUTPUT
#   plots with the distribution of the genotype frequency as a function of the allele frequency for each sample
plot_freqallele_genfreq_sample <- function(tgendata, indpopinfo) {
	pops <- unique(indpopinfo[,2])
	hetssites1 <- numeric(length(pops))
	hetssites2 <- numeric(length(pops))
	homsites <- numeric(length(pops))
	for(i in 1:length(pops)) {
		Cairo(width=10, height=10, file=paste("allfreq_vs_genfreq", pops[i],".png",sep=""), units="in", type="png", dpi=60) 
		selectinds <- which(indpopinfo[,2]==pops[i])		
		samplesize <- length(selectinds)-apply(tgendata[,selectinds],1,function(row){sum(is.na(row))})		
		freqalt <- rowSums(tgendata[,selectinds],na.rm=T)/(samplesize*2)
		print(sum(is.na(freqalt)))
		freqhets <- apply(tgendata[,selectinds],1,function(row){sum(row==1,na.rm=T)})
		freqhets <- freqhets/samplesize
		print(sum(is.na(freqhets)))
		plot(freqalt,freqhets, pch=".", main=pops[i], xlab="allele freq", ylab="het freq", ylim=c(0,1),xlim=c(0,1))
		curve(2*x*(1-x),add=T,col=4,lwd=2)
		alpha <- 0.05
		rej <- qchisq(1-(alpha), 1)
		curve(2*x*(1-x)-(2*x*(1-x)*sqrt(rej/max(samplesize))), add=T, col=4)
		curve(2*x*(1-x)+(2*x*(1-x)*sqrt(rej/max(samplesize))), add=T, col=4)
		curve(2*x,add=T, lty=2,col=4)
		curve(2-2*x,add=T, lty=2,col=4)
		dev.off()
		hetssites1[i] <- sum(freqhets>(2*freqalt*0.999), na.rm=T)
		hetssites2[i] <- sum(freqhets>(2*(1-freqalt)*0.999), na.rm=T)
		homsites[i] <- sum(freqhets<1e-5,na.rm=T)
		
	}
	list(hetssites1,hetssites2,homsites)
}


# PLOT_MISSINGDATA_PERSITE
# plot the distribution of the average missing data per site
# INPUT:
#   meanmissing : vector of size nsnps with the missing data per site
# OUTPUT:
#   histogram saved in file "missingdata_per_site.pdf"
plot_missingdata_persite <- function(missingdatapersite) {
  Cairo(width=10, height=7, file="missingdata_per_site.pdf", units="in", type="pdf")
  hist(missingdatapersite, xlim=c(0,1), main="Mean Missing data per site", prob=T, xlab="mean missing data")
  dev.off()
}


# CLASSIFY_NORTHSOUTH
# creates array with info about sampling location of each individual
classify_NorthSouth <- function(vcf.sortedindpopinfo, indexNorth, indexSouth) {
  nind <- length(vcf.sortedindpopinfo[,1])
  northsouth <- numeric(nind)
  northsouth[vcf.sortedindpopinfo[,3]==0 & vcf.sortedindpopinfo[,4] < indexNorth] <- 1
  northsouth[vcf.sortedindpopinfo[,3]==1] <- 2
  northsouth[vcf.sortedindpopinfo[,3]==0 & vcf.sortedindpopinfo[,4] > indexSouth] <- 3
  northsouth
}

# CLASSIFY_FOURGROUPS
# creates array with info about sampling location of each individual
# INPUT
#   vcf.sortedpopinfo : matrix with nind x 4 (or more cols) with 4 columns corresponding to SpecimenID, SiteName, OnOffIndex, distance from North
#   dist : numeric vector with distances from North to define groups. First index must be zero
# RETURN
#   numeric vector of size nind with 1, 2, 3, up to dist classes, with the class to which each individuals corresponds to
classify_fourgroups <- function(vcf.sortedindpopinfo, dist) {  
  classind <- numeric(length(vcf.sortedindpopinfo[,1]))  
  for(i in 1:(length(dist)-1)) {
    classind[vcf.sortedindpopinfo[,4]>=dist[i] & vcf.sortedindpopinfo[,4]<dist[i+1]] <- i    
  }
  classind
}



# PLOT_MISSINGDATA_FIS_IND
# plot the relation between missing data and FIS
# INPUT:
#   missingdata: numeric array with the mean missing data per individual sorted according to distance from the north (output plot_missingdata)
#   fis : numeric array with the FIS per individual sorted according to distance from the north (output plot_fis)
plot_missingdata_fis_ind <- function(missingdata, fis) {
  plot(missingdata, fis)
  fit <- lm(fis ~ missingdata)
  abline(coef(fit))
  text(10*min(missingdata), 0.8*max(fis),paste("P-value=",format(summary.lm(fit)$coefficients[2,4], digits=4)))
  dev.print(device=pdf, width=7, height=5, "missingdata_vs_fis_per_ind.pdf")
}



##################################################
# DATA EXPLORATORY ANALYSES
##################################################

# NEW_GT_DATASET
# create a genlight object with genotypes
# keeping only certain individuals sorted according to sorted.indexretain
# also keep info on ind id, pop info and pop distance
# INPUT:
#   gendata : matrix with nind x nspns with the genotypes coded as NA, 0, 1, 2
#   indpopinfo : data.frame with info about each individual for the individuals in gendata (nind rows x 4 columns with indID, popID, OnOff, distance)
#   eval_pops : list of pop names to remove from dataset
# OUTPUT:
#   genlight object with genotypes and other information
new_gt_dataset <- function(gendata, indinfo) {
  # create genlight object
  new("genlight", gendata, ploidy=2, pop=indinfo[,2], ind.names=indinfo[,1], other=list(pop.dist=indinfo[,4]))
}
# new_gt_dataset <- function(gendata, vcf.sortedindpopinfo, sites, commonind, eval_pops, eval_contigs) {
#   # remove individuals from pop eval_pops
#   eval <- vcf.sortedindpopinfo[,2] != eval_pops
#   # remove sites from contigs eval_contigs
#   keepsites <-  sites[,1] != eval_contigs
#   # create genlight object
#   new("genlight", tgendata[commonind$sorted.indexretainvcf[eval],keepsites], ploidy=2, pop=vcf.sortedindpopinfo[eval,2], ind.names=vcf.sortedindpopinfo[eval,1], other=list(pop.dist=vcf.sortedindpopinfo[eval,4]))
# }


# PLOT_GT_DATA
# plot general overview of genlight object
# INPUT:
#   gendata : genlight object with genotype data
#   tag : string with the tag to this specific dataset
# OUTPUT:
#   png file "gt_data_", tag, ".png"
plot_gt_data <- function(gendata, tag) {
  par(mfrow=c(1,1))
  png(width = 1800, height = 600, paste("gt_data_", tag, ".png", sep=""))
  glPlot(gendata, posi="topleft"); 
  dev.off()  
}

# GET_KEEPSITES
# Get a vector of size nsnps with TRUE/FALSE, indicating the sites to keep in the analysis
# INPUT
#   sites : list of sites with the mean coverage and variance of coverage with nsnps rows and 4 columns (CHROM, POS, MEAN_DEPTH, VAR_DEPTH)
#   eval_contigs : array of strings with contig names to remove from dataset
# OUTPUT
#   vector of size nsnps with TRUE/FALSE, indicating the sites to keep in the analysis. TRUE indicates that the sites is kept in dataset.
get_keepsites <- function(sites, eval_contigs) {  
  keepsites <- rep.int(TRUE, length(sites[,1]))
  sel_numcontigs   <- length(eval_contigs)
  for(i in 1:sel_numcontigs) { 
    keepsites[which(sites[,1]==eval_contigs[i])] <- FALSE  
  }
  keepsites
}

# FILTERDATA_MISSINGDATA_PERSITE
# returns a vector of true/false with the sites that pass a threshold of missing data
# INPUT:
#   sorted_tgendata : nsnps x nind matrix with the individual genotypes with missing data coded as NA
#   missingdata_sites : value with the threshold of missing data (only sites with less than the threshold are kept)
# OUTPUT:
#   true/false vector of size nsnps. TRUE indicates nsnps with less missingdata than missingdata_sites threshold
filterdata_missingdata_persite <- function(sorted_tgendata, missingdata_sites) {
  prop_missingdata <- apply(sorted_tgendata, 1, function(col) sum(is.na(col)))/nrow(sorted_tgendata)
  prop_missingdata < missingdata_sites
}

# UPDATE_KEEPSITES
# update the keepsites vector according to keepsites_refilter
# INPUT:
#   keepsites : true/false vector of size nsnps
#   keepsites_refilter : true/false vector of size nspns_pass_threshold, i.e. a vector of size sum(keepsites==TRUE)
# OUTPUT:
#   true/false vector of size nspns where TRUE indicates that the SNP passes all filters applied
update_keepsites <- function(keepsites, keepsites_refilter) {
  keepsites_final <- rep.int(FALSE, length(keepsites))
  keepsites_final[keepsites][keepsites_refilter] <- TRUE
  keepsites_final
}

# GET_KEEPIND
# returns a numeric vector with the index of individuals to keep
# INPUT
#   eval_pops : vector with the pops to remove
#   missingdata : threshold, such that only individuals with less than missingdata are kept
#   vcf.sortedindpopinfo : data.frame with info about each individual (nind rows x 4 columns with indID, popID, OnOff, distance)
#   missingdata.ind : numeric vector with missing data per individual (NOTE: must be sorted as in vcf.sortedindpopinfo!!)
get_keepind <- function(eval_pops, missingdata, vcf.sortedindpopinfo, missingdata.ind) {
  
  # check that all variables have the same number of individuals
  if(length(vcf.sortedindpopinfo[,1])!=length(missingdata.ind)) {
    stop("Incorrect number of individuals in input for get_keepind")
  }
  
  keepind <- rep.int(TRUE, length(vcf.sortedindpopinfo[,1]))
  sel_numpops <- length(eval_pops)
  for(i in 1:sel_numpops) {
    eval <- vcf.sortedindpopinfo[,2] == eval_pops[i] | missingdata.ind > missingdata
    keepind[which(eval)] <- FALSE
  }
  keepind
}


# SORT_TGENDATA
# Get tgendata matrix just with individuals and sites of interest using as input a matrix already sorted by individuals
#   gendata : matrix with nspns x nind with the genotypes coded as NA, 0, 1, 2
#   keepind : vector of size nind with TRUE/FALSE indicating indivuals to keep in analysis. NOTE: this is a vector of ind. sorted according to distance!
#   keepsites : vector of size nsnps with TRUE/FALSE, indicating the sites to keep in the analysis. TRUE indicates that the sites is kept in dataset.
# OUTPUT:
#   matrix with nind x nspns with the genotypes coded as NA, 0, 1, 2, with individuals sorted, excluding ind from eval_pops and snps from eval_contigs
sort_tgendata <- function(gendata, keepind, keepsites) {
  # check that all variables have the same size
  if(nrow(gendata)!=length(keepsites)) {
    stop("Incorrect number of sites in sort_tgendata function")
  }
  if(ncol(gendata)!=length(keepind)) {
    stop("Incorrect number of individuals in sort_tgendata function")
  }
  
  # remove sites from contigs eval_contigs  
  tgendata[which(keepsites),which(keepind)]    
}



# PLOT_GT_SFS
# plot the pooled sfs based on ML genotypes and assuming alternative allele is the derived and reference is the ancestral
# INPUT:
#   tgendata : matrix with nind x nspns with the genotypes
#   tag : string with the tag to this specific dataset
# OUTPUT:
#   pdf file "sfs_pooledind_", tag, ".pdf"
plot_gt_sfs <- function(tgendata, tag) {
  par(mfrow=c(1,3))
  sfspersite <- colSums(tgendata, na.rm=T)
  hist(sfspersite, breaks=seq(-1,700,by=1), main=paste(tag, "\nFreq. spectrum of alt. allele counts\n (not correcting missing data)",sep=""), xlab="counts")
  hist(sfspersite, breaks=seq(-1,700,by=1), main=paste("ZOOM - ", tag, "\nFreq. spectrum of alt. allele counts\n (not correcting missing data)",sep=""), xlab="counts", xlim=c(0,50))
  text(25,8000,paste(sum(sfspersite==0), "nsites with p=0\nTotal", sum(sfspersite>0), "snps"))
  hist(sfspersite, breaks=seq(-1,700,by=1), main=paste("TAIL - ", tag, "\nFreq. spectrum of alt. allele counts\n (not correcting missing data)",sep=""), xlab="counts", xlim=c(200,700), ylim=c(0,50))
  dev.print(device=pdf, width=10, height=7, paste("sfs_pooledind_", tag, ".pdf",sep=""))
}


# PLOT_GT_SFS_PERPOP
# plot the pooled sfs based on ML genotypes and assuming alternative allele is the derived and reference is the ancestral
# INPUT:
#   tgendata : matrix with nind x nspns with the genotypes
#   tag : string with the tag to this specific dataset
#   vcf.sortedindpopinfo : data.frame with info about each individual (nind rows x 4 columns with indID, popID, OnOff, distance)
# OUTPUT:
#   pdf file "sfs_perpop", tag,".pdf"
plot_gt_sfs_perpop <- function(tgendata, tag, vcf.sortedindpopinfo) {
  pop.names <- unique(vcf.sortedindpopinfo[,2])
  npop <- length(pop.names)
  # get the sample sizes and cumulative sample sizes for each pop
  popsamplesizes <- numeric(npop)
  cumulativesamplesizes <- numeric(npop)
  for(i in 1:npop) {
    popsamplesizes[i] <- sum(vcf.sortedindpopinfo[,2]==pop.names[i])
    cumulativesamplesizes[i] <- popsamplesizes[i]
    if(i>1) {
      cumulativesamplesizes[i] <- popsamplesizes[i]+cumulativesamplesizes[i-1]
    }
  }
  
  # get the sfs for each pop
  aux <- c(1,cumulativesamplesizes)
  sfsperpop <- matrix(0,nrow=npop,ncol=length(tgendata[1,]))
  for(i in 1:(length(aux)-1)) {
    sfsperpop[i,] <- colSums(tgendata[aux[i]:(aux[i+1]-1),,drop=F], na.rm=T)
  }
  # Plot for each pop
  par(mfrow=c(3,5))
  for(i in 1:npop) {
    hist(sfsperpop[i,], breaks=seq(-1,popsamplesizes[i]*2,by=1), main=paste(pop.names[i],"SFS\n(with missing data)"), xlab="counts")  
  }
  dev.print(device=pdf, width=14, height=10, paste("sfs_perpop_", tag,".pdf", sep=""))    
}


# PLOT_GT_HET
# plot heterozygosity estimates based on ML genotypes
#   tgendata : matrix with nind x nspns with the genotypes
#   tag : string with the tag to this specific dataset
#   vcf.sortedindpopinfo : data.frame with info about each individual (nind rows x 4 columns with indID, popID, OnOff, distance)
# OUTPUT:
#   pdf file "sfs_perpop", tag,".pdf"
plot_gt_het <- function(gendata, vcf.sortedindpopinfo, tag) {
  absfreqderivedallele <- glSum(gendata)
  missingdataperSNP <- glNA(gendata)
  
  # Estimate He as 2pq (or 1-p²-q²)
  # where p=absfreqderivedallele/(2*nind-missingdataperSNP)
  # and q=1-p
  nind <- length(vcf.sortedindpopinfo[,1])
  freqp <- absfreqderivedallele/((2*nind)-missingdataperSNP)
  he <- 2*freqp*(1-freqp)
  
  # plot
  par(mfrow=c(1,1))
  hist(he, prob=T, main="He pooled ind based on ML GT")
  dev.print(device=pdf, width=10, height=7, paste("het_pooledind_", tag, ".pdf",sep=""))
}


# Function to plot color bar
# Copied from http://stackoverflow.com/questions/9314658/colorbar-from-custom-colorramppalette
# example of call:
# color.bar(colorRampPalette(c("light green", "yellow", "orange", "red"))(100), -1)
color.bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='') {
  scale = (length(lut)-1)/(max-min)
  
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  axis(2, ticks, las=1)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
}







# PHREDTONAT
# transforms Phred scale values to natural values, replaces negative Phred values to NA, and sets values lower than machine precision to zero
# INPUT
#   phred : phred scale values (missing data represented as negative values or NA)
# OUTPUT
#   natural values, with missing data as NA and with values smaller than machine precision set to zero
phredtonat <- function(phred) {
  # replace -1 by NA
  phred[phred<0] <- NA
  phrednat <- 10^(-phred/10)
    # the values that are inferior to the machine precision are set to 0
  phrednat[phrednat<.Machine$double.eps] <- 0
  phrednat
}


# CHECK_NA_NAN_INF
# prints the number of NA, NAN and Inf values
# INPUT
#   x : matrix or numeric array to check
# OUTPUT
#  prints the number of NA, NAN and Inf values
check_na_nan_inf <- function(x) {
  print(paste("#NA=",sum(is.na(x))))
  print(paste("#NAN=",sum(is.nan(x))))
  print(paste("#Inf=",sum(is.infinite(x))))
}





#This function creates a color scale for use with the image()
#function. Input parameters should be consistent with those
#used in the corresponding image plot. The "horiz" argument
#defines whether the scale is horizonal(=TRUE) or vertical(=FALSE).
image.scale <- function(z, zlim, col = rainbow(12), breaks, horiz=TRUE, ...){
  if(!missing(breaks)){
    if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
  }
  if(missing(breaks) & !missing(zlim)){
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
  }
  if(missing(breaks) & missing(zlim)){
    zlim <- range(z, na.rm=TRUE)
    zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
    zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  }
  poly <- vector(mode="list", length(col))
  for(i in seq(poly)){
    poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
  }
  xaxt <- ifelse(horiz, "s", "n")
  yaxt <- ifelse(horiz, "n", "s")
  if(horiz){ylim<-c(0,1); xlim<-range(breaks)}
  if(!horiz){ylim<-range(breaks); xlim<-c(0,1)}
  plot(1,1,t="n",ylim=ylim, xlim=xlim, xaxt=xaxt, yaxt=yaxt, xaxs="i", yaxs="i", ...)  
  for(i in seq(poly)){
    if(horiz){
      polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
    }
    if(!horiz){
      polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
    }
  }
}

