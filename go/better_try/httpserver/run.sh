#!/bin/bash

PORT="9998 9999"

getpids() {
    for p in $PORT; do
        pid=$(netstat -apn | grep -w "127.0.0.1:$p"| sed 's/.*LISTEN \+\(.*\)\/server/\1/g')
        echo "Port-$p-PID:$pid"
    done
}

start() {
    echo "Starting services..."
    for port in $PORT; do
        (go run server.go start -port $port > log.$port &)
    done
    sleep 2
    getpids
}

stop() {
    for p in $PORT; do
        pid=$(netstat -apn | grep -w "127.0.0.1:$p"| sed 's/.*LISTEN \+\(.*\)\/server/\1/g')
        echo "Kill PID:$pid"
        kill -9 $pid
    done
}

usage() {
    echo "Usage: start/stop/r"
    exit -1
}

cmd="start"
if [ $# -gt 0 ]; then
    cmd=$1
fi

if [ "X$cmd" != "Xstart" -a "X$cmd" != "Xstop" -a "X$cmd" != "Xr" ]; then
    usage
fi

case "$cmd" in 
    "start")
        start
        ;;
    "stop")
        stop
        ;;
    "r")
        stop
        start
        ;;
esac
