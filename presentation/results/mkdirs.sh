#!/bin/bash

INCIDENTS="50 100 200 500 1000"

# make uniform pop, uniform risk
for I in $INCIDENTS; do
  dstr="unif_${I}_unif"
  echo "Preparing $dstr"
  if [ -d $dstr ]; then
    echo "Removing old experiment files from $dstr"
    rm -rf $dstr/experiment*
  else
    mkdir $dstr
  fi
  cp -a template/* $dstr
  sed -e "s/genhill(sigma=1.0)/genunif()/" -e "s/EN.i=50/EN.i=${I}/" -i.bak $dstr/experiment_setup.R && rm $dstr/experiment_setup.R.bak
done

# make uniform pop, 1 peak risk
for I in $INCIDENTS; do
  dstr="unif_${I}_1.0_1h/"
  echo "Preparing $dstr"
  if [ -d $dstr ]; then
    echo "Removing old experiment files from $dstr"
    rm -rf $dstr/experiment*
  else
    mkdir $dstr
  fi
  cp -a template/* $dstr
  sed -e "s/EN.i=50/EN.i=${I}/" -i.bak $dstr/experiment_setup.R && rm $dstr/experiment_setup.R.bak
done

# make uniform but increasing pop, 1 peak risk
for I in $INCIDENTS; do
  P=$(($I * 100))
  Pstr="$(($P / 1000))k"
  dstr="unif${Pstr}_${I}_1.0_1h/"
  echo "Preparing $dstr"
  if [ -d $dstr ]; then
    echo "Removing old experiment files from $dstr"
    rm -rf $dstr/experiment*
  else
    mkdir $dstr
  fi
  cp -a template/* $dstr
  sed -e "s/N.p=10000/N.p=${P}/" -e "s/EN.i=50/EN.i=${I}/" -i.bak $dstr/experiment_setup.R && rm $dstr/experiment_setup.R.bak
done

# make uniform pop, 1 peak risk with changing sigma
SIGMAS="0.7 1.0 1.4 2.0 2.8"
for I in $INCIDENTS; do
  for S in $SIGMAS; do
    dstr="unif_${I}_${S}_1h/"
    echo "Preparing $dstr"
    if [ -d $dstr ]; then
      echo "Removing old experiment files from $dstr"
      rm -rf $dstr/experiment*
    else
      mkdir $dstr
    fi
    cp -a template/* $dstr
    sed -e "s/genhill(sigma=1.0)/genhill(sigma=${S})/" -e "s/EN.i=50/EN.i=${I}/" -i.bak $dstr/experiment_setup.R && rm $dstr/experiment_setup.R.bak
done
done
# make uniform pop, 1 peak risk with changing sigma
SIGMAS="0.7 1.0 1.4 2.0 2.8"
for S in $SIGMAS; do
  dstr="p${S}_100_1.0_1h/"
  echo "Preparing $dstr"
  if [ -d $dstr ]; then
    echo "Removing old experiment files from $dstr"
    rm -rf $dstr/experiment*
  else
    mkdir $dstr
  fi
  cp -a template/* $dstr
  sed -e "s/#c/c/" -e "s/sigma2=1.0/sigma2=${S}/" -e "s/sigma1=1.0/sigma1=${S}/" -e "s/EN.i=50/EN.i=100/" -i.bak $dstr/experiment_setup.R && rm $dstr/experiment_setup.R.bak
done

#   incident_rate=gentwohills(center1=c(2.0,0), factor1=0.4, center2=c(-2.0,0), factor2=0.6)
#   unif_100_1_2h_1, unif_100_1_2h_2, ...
DISTS="1 2 3 4"
for D in $DISTS; do
  X=$(echo - | awk "{ printf(\"%3.1f\", $D / 2); }")
  dstr="unif_100_1_2h_${D}/"
    echo "Preparing $dstr"
  if [ -d $dstr ]; then
    echo "Removing old experiment files from $dstr"
    rm -rf $dstr/experiment*
  else
    mkdir $dstr
  fi
  cp -a template/* $dstr
  sed -e "s/genhill(sigma=1.0)/gentwohills(center1=c(${X},0), factor1=0.4, center2=c(-${X},0), factor2=0.6)/" -e "s/EN.i=50/EN.i=100/" -i.bak $dstr/experiment_setup.R && rm $dstr/experiment_setup.R.bak
done

# distance between incident peak and 
DISTS="1 2 3 4"
for D in $DISTS; do
  X=$(echo - | awk "{ printf(\"%3.1f\", $D / 2); }")
  dstr="p1.4_100_1_1h_${D}s/"
    echo "Preparing $dstr"
  if [ -d $dstr ]; then
    echo "Removing old experiment files from $dstr"
    rm -rf $dstr/experiment*
  else
    mkdir $dstr
  fi
  cp -a template/* $dstr
  sed -e "s/#c1=0.0/c1=${X}/" -e "s/#c2/c2/" -e "s/sigma1=1.0/sigma1=1.4/" -e "s/sigma2=1.0/sigma2=1.4/" -e "s/incident_rate=genhill(sigma=1.0)/incident_rate=genhill(center=c(-${X},0),sigma=1.0)/" -e "s/EN.i=50/EN.i=100/" -i.bak $dstr/experiment_setup.R && rm $dstr/experiment_setup.R.bak
done
