#!/bin/bash

filename=phone.list
i=0
j=0
k=0
l=0
m=0
n=0

>$filename
#for i in {1..6}; do
    for j in {0..9}; do
        for k in {0..9}; do
            for l in {0..9}; do
                for m in {0..9}; do
                    for n in {0..9}; do
                        echo -n "14691${i}${j}${k}${l}${m}${n}," >> $filename
                    done
                done
            done
        done
    done
#done

cp $filename ${filename}.bk
gzip ${filename}
openssl base64 -in ${filename}.gz -out ${filename}.gz.base64
