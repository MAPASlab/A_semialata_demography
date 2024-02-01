#!/bin/bash

REF="/path/to/reference/genome/directory"
DATA="/path/to/input/directory"
OUT="/path/to/output/directory"

cd ${DATA}

bcftools mpileup -a FORMAT/DP -q 20 -Q 20 -O b -f ${REF}/GCA_004135705.1_ASEM_AUS1_V1.0_genomic.fna -b ./list_bam_files.txt | bcftools call -m -O z -o ${OUT}/allsites.vcf.gz

cd ${OUT}

bcftools index allsites.vcf.gz

bcftools view -v snps -O z -o variants.vcf.gz  allsites.vcf.gz

bcftools index variants.vcf.gz

