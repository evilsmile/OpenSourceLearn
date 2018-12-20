#!/bin/bash

module=log4js
if [ ! -d node_modules/$module ]; then
    echo "Install $module ...."
    npm install $module 
else
    echo "$module installed."
fi
