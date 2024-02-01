# Vitor Sousa, 18.12.2019
# Script that reads the genotype (GT) and (DP) fields from a VCF
# and outputs the median DP and the limits of DP per individual.
# This will be done for just the individuals in the IndInfoFile.

# The command line has the following arguments:
# 1. myvcf: vcf file name
# 2. indinfofile: file with the ID of individuals and corresponding population
#                 we want to look at. This file must have two columns, 1st indID, 2nd Pop ID 
# 3. indsvcffile: file with the list of individuals in the VCF file, obtained from "grep -n -m 1 ^#CHROM vcffilename" 
# 4. mindp: minimum dp for each individual as a proportion of the individual median DP
# 5. maxdp: maximum dp for each individual as a proportion of the individual median DP
# Example of use:
# Rscript getMeanLimitsDP.r myvcf inpopinfo.txt indsvcf.txt 0.5 2.0

# REQUIRES THE FOLLOWING FILES
# - utilfunctions.r file with definition of functions

# read the command line arguments
args <- commandArgs(trailingOnly = TRUE)
vcffile <- as.character(args[1]) # vcf file name
indinfofile <- as.character(args[2]) # file name with individual information
inds_file <- as.character(args[3]) # file name with list of individuals in VCF file (obtained from "grep -n -m 1 ^#CHROM vcffilename")
mindp <- as.numeric(args[4]) # minimum DP relative to the median DP
maxdp <- as.numeric(args[5]) # maximum DP relative to the median DP
# output file tag
returnfile_tag <- paste(vcffile, "_indDP_mean_min_max", sep="") 

# print the command line options
print(paste("vcf file name:", vcffile))
print(paste("indpopinfo file name:", indinfofile))
print(paste("minimum dp:", mindp))
print(paste("maximum dp:", maxdp))
print(paste("output file tag:", returnfile_tag))

# Load the functions
source("/path/to/utilfunctions.r")

## DEBUG
# vcffile <- "dp10_filtered" # vcf file name
# indinfofile <- "IndPop_sawfly.txt" # file name with individual information
# inds_file <- "dp10_filtered_indsVCF.txt" # file name with list of individuals in VCF file (obtained from "grep -n -m 1 ^#CHROM vcffilename")
# mindp <- 0.5 # minimum DP relative to the median DP
# maxdp <- 2 # maximum DP relative to the median DP
# returnfile_tag <- paste(vcffile, "_indDP_mean_min_max.txt", sep="") 

# Read information on individuals and pop info
indpopinfo <- read.table(indinfofile, stringsAsFactors = F, header=F) 
# read individual IDs as in VCF file
indidvcf <- scan(inds_file, what="character");
# create a matrix with two columns with the indID (just because we want to use general function that require matrices, where the indID is in the 1st column)
indidvcf <- cbind(indidvcf,indidvcf)
# number of individuals in vcf file
nindvcf  <- length(indidvcf[,1]) 

# Get index of individuals common in vcf and for which we have pop info 
# (i.e. individuals found in both files)
commonind <- get_indpopinfo_indvcf(indpopinfo, indidvcf)

# Modify indpopinfo to keep only individuals in common in indpopinfo and vcf file
cols_to_keep <- c(1:length(indpopinfo[1,]))  # columns to keep from indpopinfo
vcf.indpopinfo <- indpopinfo[commonind$indexinpopinfo, cols_to_keep]
nind <- length(vcf.indpopinfo[,1]) # number of individuals

# Check if there are individuals in VCF that are not found in the indIDPop file
if(length(indidvcf[-c(commonind$indexretainvcf),])>0) {
  stop("ERROR: there are missing individuals in VCF!")
}

# Set the default values for input arguments
filename <- vcffile

# Read the genotype data
gt_file <-  paste(vcffile ,".GT", sep="");
nind_tmp <- length(scan(gt_file, nlines=1)); # number of individuals in vcf
tgendata_original <- matrix(scan(gt_file, na.strings="-1"), ncol=nind_tmp, byrow=T); # read the genotype data
nsites <- nrow(tgendata_original) # get the number of sites
rm(nind_tmp) # remove the temporary number of individuals

# Get a matrix with the missing data (i.e.genotypes that did not pass filters)
# this is important because VCF tools only updates the genotypes and not associated DP, AD and PL
missingdata <- is.na(tgendata_original)

## Read the depth of coverage
dp_file <- paste(vcffile ,".DP", sep="");
nind_tmp <- length(scan(dp_file, nlines=1));
dp <- matrix(scan(dp_file), ncol=nind_tmp, byrow=T);
# Replace DP of missing data genotypes by missing data
dp[missingdata] <- NA
# Replace DP of zero by missing data
dp[dp==0] <- NA 

## Keep only the selected individuals
tgendata <- tgendata_original[,commonind$indexretainvcf]
rm(tgendata_original)
dp <- dp[,commonind$indexretainvcf]

# Plot the distribution of DP per individual (for selected individuals)
pdf(file=paste(returnfile_tag, "_missingdata.pdf",sep=""), width=12, height=5)
par(mfrow=c(1,1))
boxplot(dp)
dev.off()

# Get the mean and median DP per individual
meandp_ind <- colMeans(dp, na.rm=T);
mediandp_ind <- apply(dp, 2, function(x) quantile(x, 0.5, na.rm=T));

# determine the minimum and maximum bound of DP for each individual
mindp_ind <- meandp_ind*mindp;
maxdp_ind <- meandp_ind*maxdp;

# Save a matrix with the median and DP limits for each individual
write.table(data.frame(ceiling(mediandp_ind),ceiling(mindp_ind),ceiling(maxdp_ind)), 
            row.names = vcf.indpopinfo[,1], 
            file=paste(returnfile_tag, ".txt", sep=""), 
            quote = F, col.names = F)

