#!/bin/bash
# Script to process VCF files, filtering out sites that:
# a) have a low coverage (defined per individual)
# b) have a high coverage (defined per individual)
# c) that deviate from Hardy-Weinberg equilibrium, showing an heterozygosity excess when pooling all individuals together
# REQUIRED FILES
# - "IndIDPop" : file with two columns with the ID of individuals and corresponding population
# - getMeanLimitsDP.r : R script used to get the MEAN DP per individual and the corresponding limits
# - createBED_hwe.r : R script used to create a BED file with the sites with an excess heterozygosity across all individuals (deviating from HWE)
# REQUIRED PROGRAMS
# - htslib, samtools, vcftools
# Author: Vitor Sousa 
# Last updated: 04/07/2021 
# Please e-mail me if you find any problems: vmsousa [at] fc.ul.pt

#######################################################################
# 1 - LOAD PROGRAMS
#######################################################################

# Here you need to load programs using the definitions of your system


#######################################################################
# 2 - SETTINGS 
#######################################################################
# These could be modified to change the settings of the analysis
# Take the arguments from the command line, with variables $1,$2...
# tag for VCF file (vcf file with format "vcffile".vcf.gz)
vcffile=$1;
# tag for the indIDpop file (file with two columns with the ID of individuals and corresponding population)
indidpop=$2; 
# minimum DP per genotype
mincoverage=$3;
# minimum DP per individual (in relation to individual mean DP, e.g. 0.5 means that it is 50% of the mean DP)
minindpd=0.5;
# maximum DP per individual (in relation to individual mean DP, e.g. 2 means that it is twice the mean DP)
maxindpd=2.0;
# tag for the population (this will be used in resulting files)
poptag="filtered";
# The script can be run at different regions, defined in BED files (e.g. coding, non-coding, etc.)
# tag of the region ("all" means that all SNPs are considered)
region="all";
# proportion of missing data per site (across all individuals) allowed (all sites with missing data per site > ms_site are removed)
ms_site=$4;
#ms_site=0.50;


#######################################################################
# 3 - CLEAN VCF - SIMPLE FILTERS TO KEEP ONLY SNPs
#######################################################################
# These filters keep only bi-allelic sites, with missing data less than ms_site
# vcftools options used:
# --minDP ${mincoverage}: keep only sites with DP>mincoverage
# --min-alleles 2 --max-alleles 2: keep only biallelic sites
# --remove-indels: remove indels
# --max-missing ${ms_site}: remove sites with missing data per site > ms_site
# --recode --recode-INFO-all --out filename: create a new vcf file after applying the filters

# check that VCF file exists and is a vcf.gz
if [[ -f ${vcffile}.vcf.gz ]]; then
	vcftools --gzvcf ${vcffile}.vcf.gz --chr "CM014270.1" --chr "CM014271.1" --chr "CM014272.1" --chr "CM014273.1" --chr "CM014274.1" --chr "CM014275.1" --chr "CM014276.1" --chr "CM014277.1" --chr "CM014278.1" --minDP ${mincoverage} --min-alleles 2 --max-alleles 2 --remove-indels --max-missing ${ms_site} --recode --recode-INFO-all --out dp${mincoverage}_${poptag}
else
	# if file not found output a message with the error
	echo "VCF file ${vcffile}.vcf.gz not found!"
fi
	
# Create a file with the individuals in the VCF file
grep -n -m 1 ^#CHROM dp${mincoverage}_${poptag}.recode.vcf > dp${mincoverage}_${poptag}_indsVCF_tmp.txt
# Remove the first 9 elements of the resulting file, since those are not individual IDs.
cut -f10- dp${mincoverage}_${poptag}_indsVCF_tmp.txt > dp${mincoverage}_${poptag}_indsVCF.txt
rm dp${mincoverage}_${poptag}_indsVCF_tmp.txt
# BGZIP the file and create a index file
bgzip -c dp${mincoverage}_${poptag}.recode.vcf > dp${mincoverage}_${poptag}.vcf.gz # compress
tabix -p vcf dp${mincoverage}_${poptag}.vcf.gz # create index file
rm dp${mincoverage}_${poptag}.recode.vcf # remove intermediate files

# Get the Genotypes, DP and PL fields discarding the sites with heterozygote excess
# Get GT field
bcftools query -f '[%GT\t]\n' dp${mincoverage}_${poptag}.vcf.gz > dp${mincoverage}_${poptag}.GT
# Replace 0/0 by 0 # Replace 0/1 and 1/0 by 1 # Replace 1/1 by 2 # Replace ./. by -1 # Replace . by -1
sed -i "s/0\/0/0/g;s/0\/1/1/g;s/1\/0/1/g;s/1\/1/2/g;s/\.\/\./-1/g;s/\./-1/g" dp${mincoverage}_${poptag}.GT
# Get DP field
bcftools query -f '[%DP\t]\n' dp${mincoverage}_${poptag}.vcf.gz > dp${mincoverage}_${poptag}.DP
sed -i "s/\.\t/0\t/g" dp${mincoverage}_${poptag}.DP


################################################
# 4 - FILTER DP based on individual variation 
################################################
# 4.1. Get the mean limits per individual
Rscript /path/to/getMeanLimitsDP.r dp${mincoverage}_${poptag} ${indidpop} dp${mincoverage}_${poptag}_indsVCF.txt ${minindpd} ${maxindpd};
# this script outputs a file with the median, min and max DP per individual with the following file name
indthresholdDP="dp${mincoverage}_${poptag}_indDP_mean_min_max.txt";
# check that file exists
if [[ ! -f ${vcffile}.vcf.gz ]]; then
	echo "File ${indthresholdDP} with mean, min and max DP limits per individual does not exist!"
fi

# 4.2 Create folder to save all the files for a given filtering option
mkdir indDP
cd indDP

# 4.3. Go through the file with the individual threshold and list of individuals to keep
#      and create a VCF for each individual, performing the DP filtering
#      VCFTOOLS is used with the following options
#      --minDP ${dpmin} --maxDP ${dpmax}: filter on DP, treating as missing data all genotypes outside this DP limits
#      --indv ${indid}: perform this for each individual
#      --recode --out ${indid}: create a resulting VCT file for each individual after filtering

# Read the file with individual IDs and corresponding median, min and max DP
while read -r indid dpmean dpmin dpmax; do
   echo "Processing individual ${indid} with DP>${dpmin} and DP<${dpmax}";
   vcftools --gzvcf ../dp${mincoverage}_${poptag}.vcf.gz --minDP ${dpmin} --maxDP ${dpmax} --indv ${indid} --recode --out ${indid};
   # compress and tabix the resulting files
   bgzip -c ${indid}.recode.vcf > ${indid}.vcf.gz;
   tabix -p vcf ${indid}.vcf.gz;
   rm ${indid}.recode.vcf;
done < ../${indthresholdDP}


################################################
# 5 - MERGE DP filtered individual VCF files
################################################
# 5.1. Go through the indthresholdDP file and get the list of individual VCF files
filename=""; # string that will store the resulting vcf file names for each individual
space=" "; # string with the delimiter to separate the vcf file names in the filename string
# Get list of individual VCF files
while read -r indid dpmean dpmin dpmax; do
   filename=${filename}${space}${indid}.vcf.gz
done < ../${indthresholdDP}

# 5.2. MERGE VCF FILES WITH BCFTOOLS
# bcftools merge
# About:   Merge multiple VCF or BCF files to create one multi-sample file combining compatible records
#         into one according to the -m option.
# Usage:   bcftools merge [options] <A.vcf.gz> <B.vcf.gz> [...]
#
# Options:
#        --use-header <file>            use the provided header
#        --print-header                 print only the merged header and exit
#    -f, --apply-filters <list>         require at least one of the listed FILTER strings (e.g. "PASS,.")
#    -i, --info-rules <tag:method,..>   rules for merging INFO fields (method is one of sum,avg,min,max,join) or "-" to turn off the default [DP:sum,DP4:sum]
#    -m, --merge <string>               merge sites with differing alleles for <snps|indels|both|all|none>, see man page for details [both]
#    -O, --output-type <b|u|z|v>        'b' compressed BCF; 'u' uncompressed BCF; 'z' compressed VCF; 'v' uncompressed VCF [v]
#    -r, --regions <reg|file>           merge in the given regions only
merged="dp${mincoverage}_${poptag}_indDP"; # Tag of the resulting VCF file
bcftools merge -m all -O z ${filename} > ${merged}.vcf.gz
tabix -p vcf ${merged}.vcf.gz

# check that the merged VCF file exists
if [[ ! -f ${merged}.vcf.gz ]]; then
	echo "VCF ${merged}.vcf.gz filtered according to DP per individual does not exist!"
fi

# 5.3. Delete the individual files
while read -r indid dpmean dpmin dpmax; do
    echo "Deleting file ${indid}.vcf.gz"
    rm ${indid}.vcf.gz
    rm ${indid}.vcf.gz.tbi
    rm ${indid}.log
done < ../${indthresholdDP}


############################################################################
# 6 - REMOVE SITES WITH MAPPING ERRORS DETECTED AS HETEROZYGOSITY EXCESS
#     Remove sites with excess Heterozygosity across all samples
#     Perform the HW in the pooled individuals to remove the sites that show het excess across all pops and samples.
#     NOTE: We are assumming that sites with heterozygosity excess across all samples are biologically unlikely, and hence
#           that such sites with het excess are due to mapping errors.
############################################################################

# 6.1. Perform HWE test
# vcftools options used:
# --hardy perform the hardy-weinberg equilibrium test
# --out filename: tag for resulting file with HWE results
vcftools --gzvcf ${merged}.vcf.gz --hardy --out ${merged}_hwe

# 6.2. call R script to create the BED files with the sites to remove 
# (showing significant het excess across all sites)
# The output of this script is a bed file nohwe_excess_${merged}_hwe.bed
# with the coordinates of the sites to remove
Rscript /path/to/createBED_hwe.r ${merged}_hwe

# 6.3. Discard the sites with heterozygote excess
# vcftools options used:
# --exclude-bed nohwe_excess_${merged}_hwe.bed perform the hardy-weinberg equilibrium test
# --recode --recode-INFO-all --out filename: tag for resulting vcf file without the sites showing Het Excess
vcftools --gzvcf ${merged}.vcf.gz --exclude-bed nohwe_excess_${merged}_hwe.bed --recode --recode-INFO-all --out filtered_${merged}_hwe

# 6.4. get information about individuals and compress VCF file
# create a file with the individuals in the VCF file
grep -n -m 1 ^#CHROM filtered_${merged}_hwe.recode.vcf > filtered_${merged}_hwe_indsVCF_tmp.txt
# remove the first 9 elements of the resulting file, since those are not individual IDs.
cut -f10- filtered_${merged}_hwe_indsVCF_tmp.txt > filtered_${merged}_hwe_indsVCF.txt
rm filtered_${merged}_hwe_indsVCF_tmp.txt # remove intermediate file
# compress BGZIP the resulting VCF file and create an index file
bgzip -c filtered_${merged}_hwe.recode.vcf > filtered_${merged}_hwe.vcf.gz
tabix -p vcf filtered_${merged}_hwe.vcf.gz
rm filtered_${merged}_hwe.recode.vcf


#####################################################
# 7 - GET the genotype matrix and other relevant information from the VCF
#####################################################

# tag for the vcffile name (you can change this if you skip any of the filtering steps)
vcftag=filtered_${merged}_hwe;

# 7.1. Get GT field
bcftools query -f '[%GT\t]\n' ${vcftag}.vcf.gz > ${vcftag}.GT
# Replace 0/0 by 0 # Replace 0/1 by 1 # Replace 1/1 by 2 # Replace ./. by -1 # Replace . by -1
sed -i "s/0\/0/0/g;s/0\/1/1/g;s/1\/0/1/g;s/1\/1/2/g;s/\.\/\./-1/g;s/\./-1/g" ${vcftag}.GT

# 7.2. Get DP field
bcftools query -f '[%DP\t]\n' ${vcftag}.vcf.gz > ${vcftag}.DP
sed -i "s/\.\t/0\t/g" ${vcftag}.DP

# 7.3. Get the CHROM POS fields
# Get the list of positions in the VCF file to compare with the list of positions in the HW test file
bcftools query -f '%CHROM\t%POS\n' ${vcftag}.vcf.gz > ${vcftag}.chrpos

# 7.4. Use vcftools to get several statitiscs
# --site-mean-depth: mean depth per site
# --het: heterozygosity per individual 
# --missing-indv: missing data per individual
# --missing-site: missing data per site
# Get the mean and variance depth of coverage per site
vcftools --gzvcf ${vcftag}.vcf.gz --site-mean-depth --out ${vcftag}
vcftools --gzvcf ${vcftag}.vcf.gz --het --out ${vcftag}
vcftools --gzvcf ${vcftag}.vcf.gz --missing-indv --out ${vcftag}
vcftools --gzvcf ${vcftag}.vcf.gz --missing-site --out ${vcftag}


#####################################################
# custom additional steps (by Graciela Sotelo)
#####################################################

# save the list of individuals and sites with missing data over a given threshold, 70% in this case
# but it could be given as another argument when calling the script (see block 2 - SETTINGS)
# proportion of missing data per individual and site allowed after performing all previous filters, just as further info
ms_final=$5;
#awk '$5 >= 0.7' ${vcftag}.imiss > imiss70.txt
awk '$5 >= x' x="$ms_final" ${vcftag}.imiss > imiss${ms_final}.txt
#awk '{print $1}' imiss70.txt > imiss70_indv.txt
awk '{print $1}' imiss${ms_final}.txt > imiss${ms_final}_indv.txt
#awk '$6 >= 0.7' ${vcftag}.lmiss > lmiss70.txt
awk '$6 >= x' x="$ms_final" ${vcftag}.lmiss > lmiss${ms_final}.txt

# option to remove those individuals with missing data (after all previous filters) over a given threshold
vcftools --gzvcf ${vcftag}.vcf.gz --remove imiss${ms_final}_indv.txt --recode --recode-INFO-all --out ${vcftag};
bgzip -c ${vcftag}.recode.vcf > ${vcffile}_dp${mincoverage}_miss${ms_site}_filt_hwe.vcf.gz;
tabix -p vcf ${vcffile}_dp${mincoverage}_miss${ms_site}_filt_hwe.vcf.gz;
rm ${vcftag}.recode.vcf;


