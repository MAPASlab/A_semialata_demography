#!/bin/bash

DATA="/path/to/input/directory"
OUT="/path/to/ouput/directory"

while read -r line
do

	samtools view -S -b -h ${OUT}/"$line"_bowtie_AUS1.sam > ${OUT}/"$line"_bowtie_AUS1.bam

	samtools view -h -bq -f2 ${OUT}/"$line"_bowtie_AUS1.bam > ${OUT}/proper_pairs/"$line"_bowtie_AUS1_unique_f2.bam

	samtools sort ${OUT}/proper_pairs/"$line"_bowtie_AUS1_unique_f2.bam > ${OUT}/proper_pairs/"$line"_bowtie_AUS1_unique_f2_sort.bam

	samtools index ${OUT}/proper_pairs/"$line"_bowtie_AUS1_unique_f2_sort.bam	
	
done < ${DATA}/list_sra_files.txt
