#!/bin/bash

DATA="/path/to/input/directory"
OUT="/path/to/output/directory"

vcftools --gzvcf ${DATA}/allsites.vcf.gz --chr "CM014270.1" --chr "CM014271.1" --chr "CM014272.1" --chr "CM014273.1" --chr "CM014274.1" --chr "CM014275.1" --chr "CM014276.1" --chr "CM014277.1" --chr "CM014278.1" --minDP 7 --min-alleles 1 --max-alleles 1 --remove-indels --max-missing 0.7 --recode --recode-INFO-all --out ${OUT}/monomorphic

vcftools --gzvcf ${DATA}/allsites.vcf.gz --chr "CM014270.1" --chr "CM014271.1" --chr "CM014272.1" --chr "CM014273.1" --chr "CM014274.1" --chr "CM014275.1" --chr "CM014276.1" --chr "CM014277.1" --chr "CM014278.1" --minDP 7 --min-alleles 2 --max-alleles 2 --remove-indels --max-missing 0.7 --recode --recode-INFO-all --out ${OUT}/biallelic

