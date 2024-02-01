#!/bin/bash

DATA="/path/to/input/directory"

files="ls *_MAFpop0.obs"
delete="ls"
files=("${files[@]/$delete}")
echo ${files[@]}

for file in ${files[@]}

do
	
	newfile=$(echo $file | awk '{split ($1, a, "_"); print a[1] "_" a[2] "_" a[3]}')
	newfile=$newfile.sfs
	sed '1,2d' $file > $newfile


done

### or directly from the command line
#sed -i '1,2d' *_MAFpop0.obs
#rename -v '_MAFpop0.obs' '.sfs' *_MAFpop0.obs


