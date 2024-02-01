# Vitor Sousa 04.07.2021
# Script that reads files with Hardy-Weinberg equilibrium results
# with the format from VCFTOOLS and outputs BED file with the sites that
# deviate from Hardy-Weinberg, showing an heterozygosity excess

# read the command line
args <- commandArgs(trailingOnly = TRUE)

# get the filename to read the hwe test
filename <- as.character(args[1]) # file name of reference table
# print the command line options
print(paste("file name with hwe output:", filename))

# Read the HW results
hwtest <- read.table(paste(filename,".hwe",sep=""), header=T, stringsAsFactors = F)
# Get distribution of the p-values
print(paste("he excess", sum(hwtest$P_HET_EXCESS<0.01)))
print(paste("he deficit", sum(hwtest$P_HET_DEFICIT<0.01)))
print(paste("he overall", sum(hwtest$P_HWE<0.01)))

# remove only the sites with excess of heterozygotes
indextoremove <- which(hwtest$P_HET_EXCESS<0.01)
# Create BED file with the sites that fail the HW test for excess of heterozygotes
# BED file has three entries:
# chromosome, position-1, position (it is position-1 because it is assumed that the first base is 0)
position <- hwtest[indextoremove,2]
bedmatrix <- matrix(c(hwtest[indextoremove,1],format(position-1, scientific = F),format(position, scientific = F)), ncol=3, byrow=F)
write("#Sites with heterozygosity excess", file=paste("nohwe_excess_",filename,".bed",sep=""))
write(t(bedmatrix), file=paste("nohwe_excess_",filename,".bed",sep=""), ncolumns = 3, append=T)

# remove sites that deviate from HWE
hist(hwtest$P_HWE)
indextoremove <- which(hwtest$P_HWE<0.01)
position <- hwtest[indextoremove,2]
bedmatrix <- matrix(c(hwtest[indextoremove,1],format(position-1, scientific = F),format(position, scientific = F)), ncol=3, byrow=F)
write("#Sites that deviate from HWE", file=paste("nohwe_",filename,".bed",sep=""))
write(t(bedmatrix), file=paste("nohwe_",filename,".bed",sep=""), ncolumns = 3, append=T)
