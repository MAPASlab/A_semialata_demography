#!/bin/bash

PREFIX="4clades"

# Get all lines with genomic data
zgrep -v "^#" $PREFIX.vcf > bs_vcf/$PREFIX.allSites

# Get the header
zgrep "^#" $PREFIX.vcf > bs_vcf/header

# Get 85 files with 1000 sites each (number 86 removed due to less sites)
split -l 1000 bs_vcf/$PREFIX.allSites bs_vcf/$PREFIX.sites.

# Generate 20 files each with randomly concatenated blocks and compute the SFS for each:
for i in {1..20}
do
  # Make a new folder for each bootstrapping iteration:
  mkdir bs$i
  cd bs$i

  # Add the header to our new bootstrapped vcf file
  cat ../bs_vcf/header > $PREFIX.bs.$i.vcf
  # Randomly add 85 blocks
  for r in {1..85}
  do
    cat `shuf -n1 -e ../bs_vcf/$PREFIX.sites.*` >> ${PREFIX}.bs.$i.vcf
  done
  # Compress the vcf file again
  gzip ${PREFIX}.bs.$i.vcf

  # Make an SFS from the new bootstrapped file
  cp ../4clades_pop.txt .
  easySFS.py -i ${PREFIX}.bs.$i.vcf.gz -p 4clades_pop.txt -a -f --dtype int --proj 20,20,20,20

  # Copy the observed SFS file into this folder renaming it to match the .tpl prefix
  cp output/fastsimcoal2/${PREFIX}_MSFS.obs  ../bs_sfs/${PREFIX}.bs.${i}_MSFS.obs

  # Say that it is finished with iteration $i
  echo bs$i" ready"

  cd ..
done
