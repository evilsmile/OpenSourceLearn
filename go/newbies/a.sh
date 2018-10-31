#!/bin/bash

url="http://www.baidu.com/index.js"
echo ${url#*/}   # 取右边
echo ${url##*/}  # 贪婪取右边
echo ${url%/*}   # 取左边
echo ${url%%/*}  # 贪婪取左边

echo ${url%:/*}
