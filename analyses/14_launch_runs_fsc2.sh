#!/bin/bash

# launch several fsc2 jobs, each from a different input file (e.g., from a different population) for a given model
# arguments given in the command line:
# input directory (where *.obs, *.tpl and *.est files are located)
WRKDIR=$1
# model to run (e.g., 1c or 2c)
MOD=$2
# column of *.ALL_param file that corresponds to "MaxEstLhood" to be sorted by 
COL=$3

FILES="ls *.obs"
delete="ls"
FILES=("${FILES[@]/$delete}")
echo ${FILES[@]}

for FILE in ${FILES[@]}
do

	REP=$(echo $FILE | awk '{split ($1, a, "_"); print a[1]}')
	sbatch run_fsc2.sh $REP ${MOD}_${REP} $WRKDIR $COL
	#echo "sbatch run_fsc2.sh $REP ${MOD}_${REP} $WRKDIR $COL"

done
