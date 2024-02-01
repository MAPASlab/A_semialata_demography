#!/bin/bash

REF="/path/to/reference/genome/directory"
DATA="/path/to/input/directory"
OUT="/path/to/ouput/directory"
CORES="number_of_cores_to_use"

while read -r line
do

	bowtie2 -x ${REF}/AUS1 --no-unal -1  ${DATA}/"$line"_1.fastq.gz -2 ${DATA}/"$line"_2.fastq.gz -p $CORES -S ${OUT}/"$line"_bowtie_AUS1.sam

done < ${DATA}/list_sra_files.txt

