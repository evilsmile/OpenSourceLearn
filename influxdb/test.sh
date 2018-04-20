#!/bin/bash

#curl -I -sl http://172.18.14.220:8086/ping

# 404?
#curl http://172.18.14.220:8086/debug/requests

# GET
#curl -G 'http://172.18.14.220:8086/query?db=eviltest' --data-urlencode 'q=select * from temperature'

#curl -XPOST 'http://172.18.14.220:8086/query?db=eviltest' --data-urlencode 'q=select * into newtemperature from temperature'

#curl -XPOST 'http://172.18.14.220:8086/query?db=eviltest' --data-urlencode 'q=select * from mymeas'

#curl -XPOST 'http://172.18.14.220:8086/query?db=eviltest' --data-urlencode 'q=CREATE DATABAS dd'

#curl -XPOST 'http://172.18.14.220:8086/query?db=eviltest&chunked=2&epoch=s&pretty=true&u=a&p=22' --data-urlencode 'q=SELECT * FROM mymeas'

#curl -XPOST -u a:22 'http://172.18.14.220:8086/query?db=eviltest&chunked=2&epoch=s&pretty=true' --data-urlencode 'q=SELECT * FROM mymeas'

#curl -G -H "Accept: application/csv" 'http://172.18.14.220:8086/query?db=eviltest&pretty=true' --data-urlencode 'q=SELECT * FROM mymeas;select * from population'

#curl -F "q=@queries.txt" -F "async=true" 'http://172.18.14.220:8086/query'

curl -G 'http://172.18.14.220:8086/query?db=eviltest' --data-urlencode 'q=select * from "temperature" where "Beijing" = $bj_value' --data-urlencode 'params={"bj_value":"3"}' 
select * from "temperature" where "Beijing" = $bj_value
