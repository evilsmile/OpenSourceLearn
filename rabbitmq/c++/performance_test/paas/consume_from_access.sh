#!/bin/bash

msg="clientId=b01221&userName=aWFuX3Rlc3Q=&smsId=d07a93a9-89d1-4883-ba81-651abd95a3e7&phone=18589060708&sign=5pGp5oucLui9pg==&content=IOadpeS6hg==&smsType=0&smsfrom=6&paytype=1&signport=&showsigntype=1&userextpendport=&longsms=0&ids=b7beb67b-d388-4d69-9cbc-147fed3bb8a6&clientcnt=1&csid=1008552&csdate=20171025114902&templateid=&temp_params=&temp_type=&test_channelid=."

#new_queues_name="lijing-send lijing-c2s-yd-hy lijing-c2s-lt-hy lijing-c2s-dx-hy lijing-c2s-hy lijing-c2s-exception-yd-hy lijing-c2s-exception-lt-hy lijing-c2s-exception-dx-hy lijing-c2s-MoAndRt lijing-c2s-db lijing-send-db lijing-c2s-http-MoAndRt c2s_report_lijing"
new_queues_name="channel_3892 channel_3893 channel_3894"
#new_queues_name="lijing-c2s-lt-hy"

for i in ${new_queues_name}; do
	exchange=$i
	queue=$i
	router=$i
	
	ratelimit=400
	
	if [ $# -gt 0 ]; then
	    ratelimit=$1
	fi
	
	cmd="./consumer -e $exchange -q $queue -r $router -R $ratelimit"
	echo $cmd
	eval $cmd
done
