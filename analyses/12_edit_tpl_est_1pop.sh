#!/bin/bash

# generate *.tpl and *.est files from each SFS file (*MAFpop0.obs) to run one-population models in fastsimcoal2
FILES="ls *MAFpop0.obs"
delete="ls"
FILES=("${FILES[@]/$delete}")
echo ${FILES[@]}

for FILE in ${FILES[@]}
do

	POP=$(echo $FILE | awk '{split ($1, a, "_"); print a[1]}')
	echo $POP
	SIZE0=$(sed 2!d $FILE | awk '{print ((NF-1))}')
	echo $SIZE0

	# model (1c or 2c) given as an argument when calling the script
	# templates available in fsc2_templates/one_pop/
	cp ${1}.est ${1}_${POP}.est
	cp ${1}.tpl ${1}_${POP}.tpl
	sed -i "s/^N0/$SIZE0/" "${1}_${POP}.tpl"

done

