#!/bin/bash
cnt=1000
if [ $# -gt 0 ]; then
    cnt=$1
fi

echo "============== repush msg to flow_ctl_test_router ===================..."
./producer -p flow_ctl_test_queue -r flow_ctl_test_router -S $cnt -s 2048;
echo "============== repush msg to flow_ctl_test_router2 ===================..."
./producer -p flow_ctl_test_queue2 -r flow_ctl_test_router2 -S $cnt -s 2048
