#!/bin/bash
cnt=1000
if [ $# -gt 0 ]; then
    cnt=$1
fi

#size=1
size=2048
#size=20480

echo "============== repush msg to flow_ctl_test_router ===================..."
./producer -p flow_ctl_test_queue -r flow_ctl_test_router -S $cnt -s $size;
#echo "============== repush msg to flow_ctl_test_router2 ===================..."
#./producer -p flow_ctl_test_queue2 -r flow_ctl_test_router2 -S $cnt -s $size
