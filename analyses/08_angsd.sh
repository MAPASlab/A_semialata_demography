#!/bin/bash

### set working directory, where bam files are located and move to it
WRKDIR="/path/to/bam/files"
cd $WRKDIR


### set variables to input/output data
## list of bam files per group
LISTS="ls list_*"
delete="ls"
LISTS=("${LISTS[@]/$delete}")
echo ${LISTS[@]}

## list of nuclear chromosomes (genome positions to analyze), e.g., "list_chr.txt"
CHR="/path/to/chromosome/file"
## reference genome data
REF="/path/to/genome/file"

## file with info on mean depth per sample for nuclear chromosomes
DP="/path/to/depth/file"

## only sites with data for at least a given percentage of the samples will be considered
PROPIND="0.7"

SUMMARY="/path/to/summary/file"
echo -n "" > $SUMMARY


### angsd commands
CORES=8
GENLIK=1
FILTERS="-minMapQ 20 -minQ 20 -skipTriallelic 1 -doCounts 1 -doHWE 1"
TODO="-doMajorMinor 1 -doMaf 1 -dosnpstat 1 -doPost 2 -doGeno 7"
TODO2="-doSaf 1 -anc $REF"


### LOOP OVER LISTS
for LIST in ${LISTS[@]}
do
	## calculate min and max depth across samples and min number of samples with data to set filters below
	Rscript "/path/to/get_depth_range.r" $LIST $DP $PROPIND
	MINDP=$(cat "/path/to/depth_range.txt" | awk '{print $1}')
	MAXDP=$(cat "/path/to/depth_range.txt" | awk '{print $2}')
	MININD=$(cat "/path/to/depth_range.txt" | awk '{print $3}')

	## create output folder and get names for output files from list filename
	CLADE=$(echo $LIST | awk '{split ($1, a, "_"); print a[2] "_" a[3]}')
	echo $CLADE
	OUTDIR="/path/to/$CLADE"
	echo $OUTDIR
	mkdir $OUTDIR
	POP=$(echo $LIST | awk '{split ($1, a, "_"); print a[2] "_" a[3] "_" a[4]}')
	echo $POP
	IND=$(cat $LIST | wc -l)
	echo $IND

	## run first set of angsd commands
	angsd -b $LIST -rf $CHR -GL $GENLIK -P $CORES $FILTERS -setMinDepth $MINDP -setMaxDepth $MAXDP -minInd $MININD $TODO -out $OUTDIR/$POP

	## filter heterozygotes excess
	zcat $OUTDIR/$POP.snpStat.gz | awk '($3+$4+$5+$6) > 0' | awk '($16)/($3+$4+$5+$6) < 0.7' | cut -f 1,2 > $OUTDIR/${POP}_sites2do

	angsd sites index $OUTDIR/${POP}_sites2do

	## run second set of angsd commands
	angsd -sites $OUTDIR/${POP}_sites2do -rf $CHR -ref $REF -b $LIST -GL $GENLIK -P $CORES $TODO2 -minInd $MININD -out $OUTDIR/${POP}_sfs 

	realSFS -fold 1 $OUTDIR/${POP}_sfs.saf.idx > $OUTDIR/$POP.sfs

	## collect results, copy sfs to summary file
	SFS=$(cat $OUTDIR/$POP.sfs)
	echo -e "$POP\t$IND\t$SFS" >> $SUMMARY

done

