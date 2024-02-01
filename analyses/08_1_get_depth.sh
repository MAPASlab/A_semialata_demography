#!/bin/bash

### set working directory, where bam files are located, and move to it
WRKDIR="/path/to/bam/files"
cd $WRKDIR


### set variables to input/output data: sorted bam files, bed file with chr positions, txt file to save mean individual depth
FILES="ls *_bowtie_AUS1_unique_f2_sort.bam"
delete="ls"
FILES=("${FILES[@]/$delete}")
echo ${FILES[@]}

CHRPOS="/path/to/list_chr_positions.bed"

SUMMARY="/path/to/depth.txt"
echo -n "" > $SUMMARY


### LOOP OVER FILES
for FILE in ${FILES[@]}
do

	MEANDP=$(samtools depth -b $CHRPOS $FILE | awk -v OFS='\t' '{sum += $3}END{print sum/NR, NR}')
	echo -e "$FILE\t$MEANDP" >> $SUMMARY

done

