编译方式：
./configure --builddir=$HOME/software-build/nginx-1.10.0 --prefix=$HOME/software-install/nginx-1.10.0 --add-module=/home/evil/workspace/OpenSourceLearn/nginx/diy_modules/hello_world
make 
make install

运行方式：
在nginx.conf中添加：
location = /hello {
    hello_world;
}

在浏览器中输入 http://localhost/hello
