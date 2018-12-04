#!/bin/bash

i=0
while true; do
	#echo "wtnhxymk$i" 
	echo -n "wtnhxymk$i" | md5 | grep "^00000"
	(( i += 1 ))
done
