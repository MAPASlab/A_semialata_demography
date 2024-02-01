#!/bin/bash

# blueprint files are located in the directory where stairwayplot is installed ("stairway_plot_v2.1.1")

files="ls *.blueprint"
delete="ls"
files=("${files[@]/$delete}")
echo ${files[@]}

for file in ${files[@]}

do

 java -cp stairway_plot_es Stairbuilder $file
 bash $file.sh

done

