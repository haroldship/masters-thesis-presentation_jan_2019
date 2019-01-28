#!/bin/bash

for d in *unif
do
  cd $d/output
  sed -i.bak -e '/Peak/d' -e '/Centroid/d' mean_table.tex
  sed -i.bak -e '/Peak/d' -e '/Centroid/d' std_table.tex
  cd ../..
done
