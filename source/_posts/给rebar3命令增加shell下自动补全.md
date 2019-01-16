---
title: erlang-solutions-国内镜像地址
date: 2019/1/16 15:30:00
categories:
- other
tags:
- erlang rebar3
---

## 前言

有时候习惯了shell下面使用自动补全功能,会发现这个功能减少了使用的学习成本,另外还大大增加了效率,平时我们在进行`erlang`开始时候,会用到`rebar3`构建工具,但是这个工具没有提供自动安装自动补全的命令,只能自己去手动安装.

## 增加自动补全

ubuntu 修改如下:

``` shell
## 下载自动补全命令
wget https://raw.githubusercontent.com/erlang/rebar3/master/priv/shell-completion/bash/rebar3 
## 复制到对应目录下
sudo cp rebar3 /etc/bash_completion.d/
## 让当前会话生效
source ~/.bashrc
```

以上修改只是`bash`的,当然`rebar3`还支持其他`shell`: `fish` /`zsh`

-------

## 参考连接

* [详解Linux Shell命令自动补全](https://blog.csdn.net/mycwq/article/details/52420330)