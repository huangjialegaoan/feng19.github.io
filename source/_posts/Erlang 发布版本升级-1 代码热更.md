---
title: Erlang 发布版本升级-1 代码热更
date: 2017/10/1 0:0:0
categories:
- Erlang-Release-Upgrade
tags:
- erlang
- release upgrade
---

# 代码热更

下面我们会展示两份代码,分别是版本1和版本2

test_load.erl (vsn-1):

```erlang
-module(test_load).
-export([print/0]).
print() ->
    io:format("vsn~p~n", [1]).
```

test_load.erl (vsn-2):

```erlang
-module(test_load).
-export([print/0]).
print() ->
    io:format("vsn~p~n", [2]).
```

两份代码仅有的差别只是打印的版本号不同,下面我们来看看怎么让运行中版本1代码热更到版本2的代码

```erlang
erlc test_load.erl %% 先编译代码
erl %% 打开eshell
1> test_load:print(). // vsn=1
vsn1
ok
2> l(test_load). // 先在外部执行erlc test_load.erl编译代码 然后再 加载新代码
{ok,test_load}
3> test_load:print(). // vsn=2
vsn2
ok
```

可以看到,只用通过执行 l(test_load). 就能直接加载新的代码了,很简单对吧,我相信这个知识点大家都懂,因为很多同学在线上紧急修复代码就是这么干的,但是这样做却很容易出问题,而且有很多很多局限性,在以后的章节我们会一起探讨这些问题,现在继续来看看代码版本方面的知识

------

# 代码版本

先问大家一个简单问题: 在erlang的vm里面,代码模块能同时存在几个版本?

答案是两个!下面我用实例来演示一遍:

code_replace.erl (vsn-1):

```erlang
-module(code_replace).
-export([start/0, loop/0]).

start() ->
    spawn(fun loop/0).
loop() ->
    receive
        _ -> loop()
    after 1500 ->
        io:format("vsn~p~n", [1]),
        loop()
    end.
```

当前我们有一份代码,这个代码主要工作就是开启一个进程,然后这个进程每隔1.5秒打印一下版本号,我们先启动一下这个进程:

```erlang
erlc code_replace.erl && erl
1> Pid = code_replace:start(). %% 开启进程
vsn1
vsn1
vsn1
...
```

可以看到我们的进程运行良好,但是突然我开始讨厌打印1了,我想让它打印2

code_replace.erl (vsn-2):

```erlang
-module(code_replace).
-export([start/0, loop/0]).

start() ->
    spawn(fun loop/0).
loop() ->
    receive
        _ -> loop()
    after 1500 ->
        io:format("vsn~p~n", [2]),  %% 修改 1 => 2
        loop()
    end.
```

先编译:

```erlang
erlc code_replace.erl
```

然后加载到vm里:

```erlang
2> l(code_replace).
vsn1
vsn1
vsn1
...
```

虽然我们加载了新代码,但是我们却看到之前我们开启的进程打印的仍然是1,难道是加载代码失败了吗?我们来再开一个进程看看:

```erlang
3> P2 = code_replace:start().
vsn2
vsn1
vsn2	
vsn1
...
```

可以看到,代码加载是有效的,而我们之前的进程运行的仍然是旧版本的代码,而新的进程运行的是新版本的代码

------

我是个多变的人~ >_ > 突然又不喜欢打印2了,我想让它打印3:

code_replace.erl (vsn-3):

```erlang
-module(code_replace).
-export([start/0, loop/0]).

start() ->
    spawn(fun loop/0).
loop() ->
    receive
        _ -> loop()
    after 1500 ->
        io:format("vsn~p~n", [3]),  %% 修改 2 => 3
        loop()
    end.
```

然后加载到vm里:

```erlang
erlc code_replace.erl
4> l(code_replace).
vsn2
vsn2
vsn2
...
```

奇怪的事情发生了

原来是vsn1和vsn2都会打印出来,但是加载新版本代码之后,vsn1的打印消失了,我们来看看他是不是被kill了

```erlang
5> erlang:is_process_alive(P). %% false
```

可以看到我们的第一个进程P已经壮烈牺牲了

接下来,问题就来了,为啥第一个进程死掉了,而第二个进程却还或者呢?

我们去官方文档看看解析:[连接](http://erlang.org/doc/reference_manual/code_loading.html#id89620)

> Erlang supports change of code in a running system. Code replacement is done on module level.
> The code of a module can exist in two variants in a system: **current** and **old**. When a module is loaded into the system for the first time, the code becomes 'current'. If then a new instance of the module is loaded, the code of the previous instance becomes 'old' and the new instance becomes 'current'.
> Both old and current code is valid, and can be evaluated concurrently. Fully qualified function calls always refer to current code. Old code can still be evaluated because of processes lingering in the old code.
> If a third instance of the module is loaded, the code server removes (purges) the old code and any processes lingering in it is terminated. Then the third instance becomes 'current' and the previously current code becomes 'old'.
> To change from old code to current code, a process must make a fully qualified function call.
> 众所周知, erlang支持运行时的代码更新,代码更新作用在模块级别.每个代码模块允许存在两个版本在系统中:当前和旧两个版本.当一个模块第一次加载进系统时,这个版本视为当前版本.如果有新的模块加载,之前的版本变为旧的版本,然后新的模块变为当前版本.两个版本都是有效的,可以同时运行.全模块调用总是指向当前版本.旧代码仍然起作用是因为进程仍然在使用着旧代码.如果第三个版本模块加载进系统时,系统会清除旧代码和关闭掉那些仍然在使用旧代码的进程.然后第三个版本会变成当前版本代码,第二个版本会变成旧版本代码.进程必须使用全模块调用才能将旧版本模块代码切换到新版本模块代码.

从这里可以看到我们之前进程做的热更其实是很危险的,万一我们热更的时候,有部分进程的驻留模块用的仍然是旧版本的模块代码,就会造成该进程被杀掉,通常在游戏服里面的表现就是全服玩家集体掉线,因为玩家进程的驻留模块还是旧的呀

那我们怎么来解决这个问题呢?

------

# 进程代码版本切换

先来看看这份代码:

code_replace_new.erl (vsn-1):

```erlang
-module(code_replace_new).
-export([start/0, loop/0]).

start() ->
    spawn(fun loop/0).
loop() ->
    receive
        code_switch ->  %% 处理消息
            ?MODULE:loop()
    after 1500 ->
        io:format("vsn~p~n", [1]),
        loop()
    end.
```

这份代码在原来的基础上加了接受code_switch消息之后,调用一下?MODULE:loop(),这个就是官方说的全模块调用,执行完这一步之后,进程的驻留模块就会从旧版本变更为新版本,下面我们用实际操作来看一下:

启动的方法还是跟之前一样,这里就不重复了

启动之后,我们将打印变为2,然后编译,然后加载,发现还是打印1

```erlang
io:format("vsn~p~n", [2]), %% 1 => 2
```

不急,我们还没有给进程发送消息让它变更

```erlang
3> P ! code_switch.
vsn2
vsn2
...
```

更新成功!!

这样我们就成功将进程的驻留模块从版本一替换到版本二了,也很好的解决了进程被杀掉的问题,那是不是我们每一个进程都需要写这样一个代码切换的代码呢,那岂不是很麻烦,

其实otp已经帮我们想好了解决方法,就是gen_server,这个在之后的章节再来详细看看

------

今天就这样~玩得开心~

end
