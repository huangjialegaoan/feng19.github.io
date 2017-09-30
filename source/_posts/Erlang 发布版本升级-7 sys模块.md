---
title: Erlang 发布版本升级-7 sys模块
date: 2017/10/1 0:0:0
categories:
- Erlang-Release-Upgrade
tags:
- erlang
- release upgrade
---

第二章的时候,我们已经接触过sys模块了,现在我们来进一步看一下源码的实现.

------

我们还是用回第二章的例子:

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

先从进程启动的位置开始切入:

gen_server.erl:

```erlang
start_link(Mod, Args, Options) ->
    gen:start(?MODULE, link, Mod, Args, Options).
```

进入gen模块最后会回调本模块的init_it方法:

```erlang
init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, Parent, Name0, Mod, Args, Options) ->
    Name = gen:name(Name0),
    Debug = gen:debug_options(Name, Options),
    case catch Mod:init(Args) of
   {ok, State} ->
       proc_lib:init_ack(Starter, {ok, self()}),      
       loop(Parent, Name, State, Mod, infinity, Debug);
   {ok, State, Timeout} ->
       proc_lib:init_ack(Starter, {ok, self()}),      
       loop(Parent, Name, State, Mod, Timeout, Debug);
   {stop, Reason} ->
       %% For consistency, we must make sure that the
       %% registered name (if any) is unregistered before
       %% the parent process is notified about the failure.
       %% (Otherwise, the parent process could get
       %% an 'already_started' error if it immediately
       %% tried starting the process again.)
       gen:unregister_name(Name0),
       proc_lib:init_ack(Starter, {error, Reason}),
       exit(Reason);
   ignore ->
       gen:unregister_name(Name0),
       proc_lib:init_ack(Starter, ignore),
       exit(normal);
   {'EXIT', Reason} ->
       gen:unregister_name(Name0),
       proc_lib:init_ack(Starter, {error, Reason}),
       exit(Reason);
   Else ->
       Error = {bad_return_value, Else},
       proc_lib:init_ack(Starter, {error, Error}),
       exit(Error)
    end.
```

进入init_it之后会调用回调模块的init方法回去初始状态,然后进入loop:

```erlang
loop(Parent, Name, State, Mod, hibernate, Debug) ->
    proc_lib:hibernate(?MODULE,wake_hib,[Parent, Name, State, Mod, Debug]);
loop(Parent, Name, State, Mod, Time, Debug) ->
    Msg = receive
	      Input ->
		    Input
	  after Time ->
		  timeout
	  end,
    decode_msg(Msg, Parent, Name, State, Mod, Time, Debug, false).
```

进入loop之后是等待消息,当接受到消息之后会进入decode_msg处理:

```erlang
decode_msg(Msg, Parent, Name, State, Mod, Time, Debug, Hib) ->
    case Msg of
	{system, From, Req} ->
	    sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
				  [Name, State, Mod, Time], Hib);
	{'EXIT', Parent, Reason} ->
	    terminate(Reason, Name, Msg, Mod, State, Debug);
	_Msg when Debug =:= [] ->
	    handle_msg(Msg, Parent, Name, State, Mod);
	_Msg ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3,
				      Name, {in, Msg}),
	    handle_msg(Msg, Parent, Name, State, Mod, Debug1)
    end.
```

接受到系统消息`{system, From, Req}`会调用`sys:handle_system_msg`方法处理,如果是普通消息会进入`handle_msg`处理,处理完普通消息最后会回到`loop`,现在我们来看看`sys:suspend/1` 做了什么:

```erlang
suspend(Name) -> send_system_msg(Name, suspend). %% 发送一个系统消息到进程
...
send_system_msg(Name, Request) ->
    case catch gen:call(Name, system, Request) of
	{ok,Res} -> Res;
	{'EXIT', Reason} -> exit({Reason, mfa(Name, Request)})
    end.
```

就是发送一个系统消息给进程,通过之前的分析,我们知道gen_server处理系统消息是通过调用`sys:handle_system_msg`方法来处理的,直接看调用`sys:handle_system_msg`方法:

```erlang
handle_system_msg(Msg, From, Parent, Mod, Debug, Misc, Hib) ->
   handle_system_msg(running, Msg, From, Parent, Mod, Debug, Misc, Hib).

handle_system_msg(SysState, Msg, From, Parent, Mod, Debug, Misc, Hib) ->
    case do_cmd(SysState, Msg, Parent, Mod, Debug, Misc) of %% Msg = suspend
	{suspended, Reply, NDebug, NMisc} ->
	    _ = gen:reply(From, Reply),
	    suspend_loop(suspended, Parent, Mod, NDebug, NMisc, Hib); %% 进入suspend_loop
	{running, Reply, NDebug, NMisc} ->
	    _ = gen:reply(From, Reply),
            Mod:system_continue(Parent, NDebug, NMisc);
	{{terminating, Reason}, Reply, NDebug, NMisc} ->
	    _ = gen:reply(From, Reply),
	    Mod:system_terminate(Reason, Parent, NDebug, NMisc)
    end.
```

进入`do_cmd`:

```erlang
do_cmd(_, suspend, _Parent, _Mod, Debug, Misc) ->
    {suspended, ok, Debug, Misc};
```

返回一个状态`suspended`:然后进入`suspend_loop`:

```erlang
%% 进入 suspend_loop 之后就停留在receive上,只处理系统消息和异常,不再处理其他消息也就是暂停处理其他消息了
%% 也就是从gen_server:loop转换到sys:suspend_loop
suspend_loop(SysState, Parent, Mod, Debug, Misc, Hib) ->
    case Hib of
	true ->
	   suspend_loop_hib(SysState, Parent, Mod, Debug, Misc, Hib);
	_ ->
	    receive
		{system, From, Msg} ->
		    handle_system_msg(SysState, Msg, From, Parent, Mod, Debug, Misc, Hib);
		{'EXIT', Parent, Reason} ->
		    Mod:system_terminate(Reason, Parent, Debug, Misc)
	    end
    end.
```

从源代码中我们可以看到 `sys:suspend/1`  的主要认为就是从`gen_server:loop`转换到`sys:suspend_loop` ,而这个方法只处理了系统消息和异常退出消息,其他消息是不出理的,其他消息会留在邮箱里,这也就完成了暂停了.

这时候我们应该执行加载新代码模块进入vm:`l(sync_code_reload).`;然后再调用`sys:change_code/4` 通知到进程去回调模块的`code_change`方法对内部状态做相应的改变:

`sys.erl`:

```erlang
change_code(Name, Mod, Vsn, Extra) -> %% 仍然是发送系统消息
    send_system_msg(Name, {change_code, Mod, Vsn, Extra}).
...
%% 从handle_system_msg进入do_cmd
do_cmd(suspended, {change_code, Module, Vsn, Extra}, _Parent,
       Mod, Debug, Misc) ->
    {Res, NMisc} = do_change_code(Mod, Module, Vsn, Extra, Misc), %% 进入 do_change_code
    {suspended, Res, Debug, NMisc};
do_cmd(SysState, Other, _Parent, _Mod, Debug, Misc) ->
    {SysState, {error, {unknown_system_msg, Other}}, Debug, Misc}.
...
do_change_code(Mod, Module, Vsn, Extra, Misc) -> %% 这里 Mod=gen_server
    case catch Mod:system_code_change(Misc, Module, Vsn, Extra) of
	{ok, NMisc} -> {ok, NMisc};
	Else -> {{error, Else}, Misc}
    end.
```

在处理change_code的时候会先调用`gen_server:system_code_change/4`:

```erlang
system_code_change([Name, State, Mod, Time], _Module, OldVsn, Extra) ->
    case catch Mod:code_change(OldVsn, State, Extra) of  %% 终于看到我们熟悉的code_change/3
   {ok, NewState} -> {ok, [Name, NewState, Mod, Time]}; %% 使用返回的新的NewState
   Else -> Else
    end.
%% 用之前的例子来说这里 Mod=sync_code_reload
```

执行完code_change之后我们获得了新的内部状态,这样就避免了与新代码的冲突了,最后需要恢复一下进程,然后正常处理消息,调用`sys:resume`:

```erlang
resume(Name) -> send_system_msg(Name, resume). %% 同样是发送系统消息
...
handle_system_msg(SysState, Msg, From, Parent, Mod, Debug, Misc, Hib) ->
    case do_cmd(SysState, Msg, Parent, Mod, Debug, Misc) of  %% 进入do_cmd, Msg=resume
	{suspended, Reply, NDebug, NMisc} ->
	    _ = gen:reply(From, Reply),
	    suspend_loop(suspended, Parent, Mod, NDebug, NMisc, Hib);
	{running, Reply, NDebug, NMisc} -> %% running
	    _ = gen:reply(From, Reply),
            Mod:system_continue(Parent, NDebug, NMisc);  %% 回调system_continue, Mod=gen_server
	{{terminating, Reason}, Reply, NDebug, NMisc} ->
	    _ = gen:reply(From, Reply),
	    Mod:system_terminate(Reason, Parent, NDebug, NMisc)
    end.
...
do_cmd(_, resume, _Parent, _Mod, Debug, Misc) -> %% resume
    {running, ok, Debug, Misc}; %% 返回 running
do_cmd(SysState, get_state, _Parent, Mod, Debug, Misc) ->
    {SysState, do_get_state(Mod, Misc), Debug, Misc};
...
```

恢复状态为running后,会回调`gen_server:system_continue`方法:

```erlang
system_continue(Parent, Debug, [Name, State, Mod, Time]) -> %% 回调
    loop(Parent, Name, State, Mod, Time, Debug). %% 从新进入loop
```

这个方法主要做的是带着新的state进入loop,进入loop之后会正常的接受和处理消息.

以上就是内部状态变更的整个过程了.

到这来整个<<erlang发布版本升级>>的章节也到此为止了,有什么问题联系本人,谢谢大家!

end
