#!/bin/bash

# 在$HOME目录 下生成 .dialyzer_plt文件，以便后面的分析 
# dialyzer --build_plt -r /opt/otp_20.0/lib/erlang/lib/erts-9.0/ebin/ /opt/otp_20.0/lib/erlang/lib/kernel-5.3/ebin/ /opt/otp_20.0/lib/erlang/lib/stdlib-3.4/ebin/ /opt/otp_20.0/lib/erlang/lib/mnesia-4.15/ /opt/otp_20.0/lib/erlang/lib/sasl-3.0.4/

dialyzer ./fib.erl
