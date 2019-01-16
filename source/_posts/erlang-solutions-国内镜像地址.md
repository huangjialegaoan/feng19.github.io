---
title: erlang-solutions-国内镜像地址
date: 2018/4/8 16:00:00
categories:
- other
tags:
- erlang
---

有部分开发erlang的同学都是用[erlang-solutions](https://www.erlang-solutions.com/resources/download.html)网站提供的erlang来安装的吧
但是在国内用这个站点的仓库地址网速烂到爆,只能用配合梯子来使用
偶然的机遇,我发现[清华大学开源软件镜像站](https://mirrors.tuna.tsinghua.edu.cn)也有做这个站点的仓库的镜像,[地址](https://mirrors.tuna.tsinghua.edu.cn/erlang-solutions/)
哈哈,以后不用一升级erlang就皱眉了

ubuntu 修改如下:

`sudo vi /etc/apt/sources.list`

`16.04` : 

```erlang
# esl
#deb https://packages.erlang-solutions.com/ubuntu xenial contrib
# qing hua
deb https://mirrors.tuna.tsinghua.edu.cn/erlang-solutions/ubuntu/ xenial contrib
```

`18.04` : 

```erlang
# esl
#deb https://packages.erlang-solutions.com/ubuntu bionic contrib
# qing hua
deb https://mirrors.tuna.tsinghua.edu.cn/erlang-solutions/ubuntu/ bionic contrib
```

-------

今天就这样~玩得开心!

end
