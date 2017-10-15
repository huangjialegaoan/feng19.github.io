---
title: Erlang 发布版本升级-2 gen_server热更
date: 2017/10/1 0:0:2
categories:
- Erlang-Release-Upgrade
tags:
- erlang
- release upgrade
---

Otp的gen_server模块,我想应该是大家在erlang开发中接触最多的模块了,但是我们经常会碰到要修改内部状态(state)的时候,例如原来的state不再适用于新的需求,需要改变state的数据结构,增加或者减少一个字段

遇到这样情况之后,我想很大一部分同学都会选择关闭这个进程,然后加载新代码,然后再开启这个进程,但是面对运行中的环境,要做这个操作只能重启vm了

其实我们并不需要重启vm,只需要多动动手指敲多几行代码就能完成这个代码的热更了

------

# sys模块的api

首先先带大家认识一下这个sys模块的api:

- sys:get_state(Name) -> State. 获取进程的state
- sys:suspend(Name) -> ok. 暂停进程
- sys:change_code(Name, Module, OldVsn, Extra) -> ok | {error, Reason}. 更新代码之后告诉进程进行内部状态变更
- sys:resume(Name) -> ok. 恢复进程

之后会用到这些api,接下来,我会演示一下怎么用这些api去更新一个进程的内部状态:

------

# 实例演示

sync_code_reload.erl (vsn-1):

```erlang
-module(sync_code_reload).
-behaviour(gen_server).

-export([start/0, vsn/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
    terminate/2, code_change/3]).

-record(state, {}).
start() ->
    gen_server:start(?MODULE, [], []).
vsn() -> 1.
init([]) ->
    {ok, #state{}}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast(_Request, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, {state}, _Extra) ->
    io:format("old:~p, ex:~p~n", [_OldVsn, _Extra]),
    {ok, #state{}}.
```

这份代码很简单,什么东西都没有处理,这就是第一个版本

先启动进程:

```erlang
1> {ok, P} = sync_code_reload:start().
```

进程启动完成,之后万恶的的策划给我们加了一个需求,需要增加一个字段id来识别这个进程,而且更新这个还不能重启vm,不然他们就有借口拿我们程序员祭天了,但是这难不倒我们,对模块进行简单修改之后:

sync_code_reload.erl (vsn-2):

```erlang
-module(sync_code_reload).
-behaviour(gen_server).
-export([start/0, vsn/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
    terminate/2, code_change/3]).

-record(state, {a}).
start() ->
    gen_server:start(?MODULE, [], []).
vsn() -> 2. %% 1 => 2
init([]) ->
    {ok, #state{}}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast(_Request, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, {state}, _Extra) ->
    io:format("old:~p, ex:~p~n", [_OldVsn, _Extra]),
    {ok, #state{id = 1}}. %% 增加 id = 1
```

我们先来看看这个进程的内部状态,还记得之前说的api吧,现在排上用场了:

```erlang
2> sys:get_state(P).
{state}
```

首先我们需要先暂停这个进程一小会:

```erlang
3> sys:suspend(P).
ok
```

然后加载之前编译好的代码:

```erlang
4> l(sync_code_reload).
{module,sync_code_reload}
```

下一步,我们通知一下进程进行内部状态的变更:

```erlang
5> sys:change_code(P, sync_code_reload, "0.1.1", []).  
old:"0.1.1", ex:[]
ok
```

返回ok表示成功,然后我们再来看看状态:

```erlang
6> sys:get_state(P).
{state,1}
```

ok,新的字段已经添加成功了,接下来,我们恢复这个进程的运行:

```erlang
7> sys:resume(P).
```

done!

整个流程下来,我想就算你用了机械键盘,速度很快,很666,Duang~Duang~Duang~,但是你还是会被拿去祭天的,因为,整个过程肯定超过10秒,如果是游戏服,这个进程是游戏进程,那么你的玩家用户就整整10秒没有响应,因为你暂停了呀~

其实你可以这样:

```erlang
sys:suspend(P),l(sync_code_reload),sys:change_code(P, sync_code_reload, "0.1.1", []),sys:resume(P).
```

将操作连在一起操作,肯定不会超过0.1秒,除非你在你的回调方法code_change里执行了timer:sleep(999999).这样做的话,上天也救不了你~

> 友情提示:
> 不想被拉去祭天,就别在code_change里面执行耗时的操作!!!
> 不想被拉去祭天,就别在code_change里面执行耗时的操作!!!
> 不想被拉去祭天,就别在code_change里面执行耗时的操作!!!

说三遍~你懂得!

------

另外如果我们有很多个进程,很多个模块需要做进程内部状态的热更呢?

难道每一个进程都做一次上面的操作吗?显然是不可能的~

之后的章节我们会来讲讲怎么用otp的方式来更新一个发布版本!!

------

今天就这样!玩得开心~

end
