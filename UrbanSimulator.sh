#!/bin/sh
# edit configuration file with port in input
regex_for_numbers='^[0-9]+$' 
if [ "$#" -ne 1 ] || ! [ "$1" =~ $re]; then
  echo "Inserisci la porta da utilizzare come argomento"
  exit 1
fi