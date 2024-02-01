#!/bin/bash

# get info from *.sfs files to edit template.blueprint file before running one-population models in Stairway Plot 2

DATA="/path/to/input/directory"

files="ls *.sfs"
delete="ls"
files=("${files[@]/$delete}")
echo ${files[@]}

for file in ${files[@]}

do

	Rscript "$DATA/get_sfs_info.r" $file
	POP=$(cat "$DATA/sfs_info.txt" | awk '{print $1}')
	echo $POP
	SEQ=$(cat "$DATA/sfs_info.txt" | awk '{print $2}')
	echo $SEQ
	LEN=$(cat "$DATA/sfs_info.txt" | awk '{print $3}')
	echo $LEN
	BREAKS=$(cat "$DATA/sfs_info.txt" | awk '{print $4 " " $5 " " $6 " " $7}')
	echo $BREAKS
	FREQ=$(cat "$DATA/sfs_info.txt" | awk '{for(i=8;i<=NF;i++) printf $i" "; print ""}')
	echo $FREQ

	# template.blueprint available in stairwayplot2_templates/
	cp template.blueprint $POP.blueprint
	sed -i "s/^popid:/popid: $POP/" "$POP.blueprint"
	sed -i "s/^nseq:/nseq: $SEQ/" "$POP.blueprint"
	sed -i "s/^L:/L: $LEN/" "$POP.blueprint"
	sed -i "s/^SFS:/SFS: $FREQ/" "$POP.blueprint"
	sed -i "s/^nrand:/nrand: $BREAKS/" "$POP.blueprint"
	sed -i "s/^project_dir:/project_dir: $POP/" "$POP.blueprint"
	sed -i "s/^plot_title:/plot_title: $POP/" "$POP.blueprint"

done

