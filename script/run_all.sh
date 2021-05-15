#!/bin/bash

MAINR="../main_markov_chain.R"

for i in {12..15}
do
  sed -i 1"s/.*/#&/" $MAINR
  sed -i "1isource('../static_mtx_ex${i}.R')" $MAINR
  Rscript $MAINR
done
