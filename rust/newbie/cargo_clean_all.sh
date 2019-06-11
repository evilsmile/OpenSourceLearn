#!/bin/bash
for i in $(ls -1); do 
    echo $i; 
    if [ -d $i ]; then 
        cd $i && cargo clean && cd - 
    fi
done
