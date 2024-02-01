#!/bin/bash

DATA="/path/to/input/directory"
OUT="/path/to/output/directory"

cd $DATA

## tag for vcf file, e.g., for filtered.vcf.gz
vcffile="filtered"

## list of number of individuals for downsampling, e.g., 10 individuals
SSizes=(10)

for size in ${SSizes[@]}
do

projection=$((size*2))

## select command line below based on number of populations included in vcf file (one, two or four)
# for a single population, uncomment next line
easySFS.py -i ${vcffile}.vcf.gz -p pop_file.txt --proj ${projection} -a -o ${OUT}/${vcffile}_ind${size} --prefix ${vcffile}_ind${size}
# for two populations, uncomment next line
#easySFS.py -i ${vcffile}.vcf.gz -p pop_file.txt --proj ${projection}, ${projection} -a -o ${OUT}/${vcffile}_ind${size} --prefix ${vcffile}_ind${size}
# for four populations, uncomment next line
#easySFS.py -i ${vcffile}.vcf.gz -p pop_file.txt --proj ${projection}, ${projection}, ${projection}, ${projection} -a -o ${OUT}/${vcffile}_ind${size} --dtype int --prefix ${vcffile}_ind${size}

done

