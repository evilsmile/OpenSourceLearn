#!/bin/bash

msg="insert into t_sms_access_3_20171027(id,content,srcphone,phone,smscnt,smsindex,sign,submitid,smsid,clientid,operatorstype,smsfrom,state,errorcode,date,channelid,smstype,charge_num,paytype,agent_id,username ,isoverratecharge,uid,showsigntype,product_type,c2s_id,process_times,longsms,channelcnt,template_id,temp_params,sid,belong_sale,sub_id)   values('bb7be735-76e4-4048-91d2-fab0e5cdee6b', '【摩拜.车】 来了', '402', '18589060708', '1', '1', '摩拜.车', '0', 'bf94eeeb-8bec-41b1-a67c-ce0372158943', 'b01221', '2','6', '0', '', '20171027103548','0','0','1','1','2016090024','ian_test','0','1234567890','1','99','1008552','0','0','1', NULL, '', ' ', 149 ,'0');RabbitMQFlag=bb7be735-76e4-4048-91d2-fab0e5cdee6b"

#name="lijing-c2s-lt-hy"
name="lijing-c2s-db"
exchange=$name
queue=$name
router=$name

ratelimit=400

if [ $# -gt 0 ]; then
    ratelimit=$1
fi

cmd='./producer -e $exchange -q $queue -r $router -m "$msg" -R $ratelimit'
echo $cmd
eval $cmd
