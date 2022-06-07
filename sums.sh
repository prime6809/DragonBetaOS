#!/bin/bash

DDPATH=../cmds
CMDS=./cmds
NOTFOUND=""

for i in $DDPATH/* 
do
  CMD=$(basename "$i")
  CMDP=$CMDS/$CMD
  if [ -e $CMDP ]
  then
    SUM1=$(md5sum "$i")
    SUM2=$(md5sum "$CMDP")
    SUM1=${SUM1:0:32}
    SUM2=${SUM2:0:32}
    if [ "$SUM1" = "$SUM2" ]
    then
      echo "$CMD matches"
    else
      echo "$CMD failed : $SUM1 $SUM2"
    fi
  else
    NOTFOUND="$NOTFOUND $CMD"
  fi
done 

echo "Files not found : $NOTFOUND"  
