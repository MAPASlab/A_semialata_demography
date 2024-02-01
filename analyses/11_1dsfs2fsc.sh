#!/bin/bash

# convert *.sfs files obtained with angsd to fastsimcoal2 format (*_MAFpop0.obs), to run one-population models 
FILES="ls *.sfs"
delete="ls"
FILES=("${FILES[@]/$delete}")
echo ${FILES[@]}

for FILE in ${FILES[@]}
do

	POP=$(echo $FILE | awk '{split ($1, a, "_"); print a[3]}')
	echo $POP
	IND=$(cat $FILE | wc -w)
	IND=$((IND-1))
	echo $IND
	FREQ=()
	echo $FREQ
	for i in $(seq -s " " $IND)
	do
		FREQ+="d0_${i} "
	done	
	FREQ=$(echo ${FREQ[@]})
	echo $FREQ
	MAIN=$(cat $FILE)

	# template_1DSFS.obs available in /fsc2_templates/one_pop/
	cp template_1DSFS.obs ${POP}_MAFpop0.obs
	sed -i "s/^d0_0/d0_0 $FREQ/" "${POP}_MAFpop0.obs"
	sed -i "s/^L3/$MAIN/" "${POP}_MAFpop0.obs"

done

