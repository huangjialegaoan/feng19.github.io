---
title: Erlang 发布版本升级-6 升级模块与进程
date: 2017/10/1 0:0:0
categories:
- Erlang-Release-Upgrade
tags:
- erlang
- release upgrade
---

或许有些同学会很容易搞混,在升级中到底是升级进程还是升级代码模块.

答案显然是肯定的,升级代码模块,而我们说的升级进程,只是在需要同步升级进程内部状态的时候才需要,而同步更新也是先加载更新新的代码模块,然后再通知进程执行内部状态的转变.

只是,我们都知道我们appup里面只有提到模块,里面并没有提及到进程,而我们都知道进程的标示pid,都是动态的,而我们在做同步代码热更的时候,是怎么将模块与进程管理起来的呢?下面我带大家从源码中找出答案.



# 进程模块关系

之前的章节我们有提到过,指令分两种:

> OTP supports a set of **release handling instructions** that are used when creating .appup files. The release handler understands a subset of these, the **low-level** instructions. To make it easier for the user, there are also a number of **high-level** instructions, which are translated to low-level instructions by systools:make_relup.
>
> 普通(High-Level)指令和底层(Low-Level)指令的区别: 普通指令是提供给用户使用的,而底层指令是通过普通指令转换而来的
>
> 我们之前生产的`*.appup`文件, 会在版本发布前的relup阶段,通过调用`systools:make_relup`将全部普通指令转换为底层指令

我们先来看看第一种:

```erlang
%% appup generated for rus_gen_server by rebar3_appup_plugin (2017/09/18 14:46:53)
{ "0.2.0",
    [{ "0.1.0",
        [{update,rus_gen_server,{advanced,[]},brutal_purge,brutal_purge,[]}] }],
    [{ "0.1.0",
        [{update,rus_gen_server,{advanced,[]},brutal_purge,brutal_purge,[]}] }]
}.
```

然后看看转换之后的relup:

```erlang
{"0.2.0",
 [{"0.1.0",[],
   [{load_object_code,{rus_gen_server,"0.2.0",[rus_gen_server]}},
    point_of_no_return,
    {suspend,[rus_gen_server]},
    {load,{rus_gen_server,brutal_purge,brutal_purge}},
    {code_change,up,[{rus_gen_server,[]}]},
    {resume,[rus_gen_server]}]}],
 [{"0.1.0",[],
   [{load_object_code,{rus_gen_server,"0.1.0",[rus_gen_server]}},
    point_of_no_return,
    {suspend,[rus_gen_server]},
    {code_change,down,[{rus_gen_server,[]}]},
    {load,{rus_gen_server,brutal_purge,brutal_purge}},
    {resume,[rus_gen_server]}]}]}. 
```

已经转换成底层指令了,接下来我们看一下在更新的时候,系统是怎么执行这些指令的:

我们从暂停(suspend)指令开始看起:

```erlang
eval({suspend, Modules}, EvalState) ->
    Procs = get_supervised_procs(),
    NewSuspended =
	lists:foldl(fun(ModSpec, Suspended) ->
			    {Module, Def} = case ModSpec of 
						{Mod, ModTimeout} ->
						    {Mod, ModTimeout};
						Mod ->
						    {Mod, default}
					    end,
			    Timeout = get_opt(suspend_timeout, EvalState, Def),
			    Pids = suspend(Module, Procs, Timeout),
			    [{Module, Pids} | Suspended]
		    end,
		    EvalState#eval_state.suspended,
		    Modules),
    EvalState#eval_state{suspended = NewSuspended};
```

可以看到这个方法最主要的方法就是通过调用方法`get_supervised_procs()` 获得了模块与进程的一个对应关系,然后保存进了内部状态里,之后还会用到.

我们再来看看`get_supervised_procs()` 这个方法做了什么事情:

```erlang
get_supervised_procs() -> %% 查找到应用下所有的sup进程
    lists:foldl(
      fun(Application, Procs) ->
              get_master_procs(Application,
                               Procs,
                               application_controller:get_master(Application))
      end,
      [],
      get_application_names()).

get_supervised_procs(_, Root, Procs, {ok, SupMod}) ->
    get_procs(maybe_supervisor_which_children(Root, SupMod, Root), Root) ++
        [{undefined, undefined, Root, [SupMod]} |  Procs];
get_supervised_procs(Application, Root, Procs, {error, _}) ->
    error_logger:error_msg("release_handler: cannot find top supervisor for "
                           "application ~w~n", [Application]),
    get_procs(maybe_supervisor_which_children(Root, Application, Root), Root) ++ Procs.

get_application_names() ->
    lists:map(fun({Application, _Name, _Vsn}) ->
                      Application
              end,
              application:which_applications()).

get_master_procs(Application, Procs, Pid) when is_pid(Pid) ->
    {Root, _AppMod} = application_master:get_child(Pid),
    get_supervised_procs(Application, Root, Procs, get_supervisor_module(Root));
get_master_procs(_, Procs, _) ->
    Procs.

get_procs([{Name, Pid, worker, dynamic} | T], Sup) when is_pid(Pid) -> %% Modules=dynamic
    Mods = maybe_get_dynamic_mods(Name, Pid),
    [{Sup, Name, Pid, Mods} | get_procs(T, Sup)];
get_procs([{Name, Pid, worker, Mods} | T], Sup) when is_pid(Pid), is_list(Mods) -> %% 动态去获取Modules
    [{Sup, Name, Pid, Mods} | get_procs(T, Sup)];
get_procs([{Name, Pid, supervisor, Mods} | T], Sup) when is_pid(Pid) ->
    [{Sup, Name, Pid, Mods} | get_procs(T, Sup)] ++
        get_procs(maybe_supervisor_which_children(Pid, Name, Pid), Pid);
get_procs([_H | T], Sup) ->
    get_procs(T, Sup);
get_procs(_, _Sup) ->
    [].

maybe_supervisor_which_children(Proc, Name, Pid) ->
    case get_proc_state(Proc) of
        noproc ->
            %% process exited before we could interrogate it.
            %% not necessarily a bug, but reporting a warning as a curiosity.
            error_logger:warning_msg("release_handler: a process (~p) exited"
                                     " during supervision tree interrogation."
                                     " Continuing ...~n", [Proc]),
            [];

        suspended ->
            error_logger:error_msg("release_handler: a which_children call"
                                   " to ~p (~w) was avoided. This supervisor"
                                   " is suspended and should likely be upgraded"
                                   " differently. Exiting ...~n", [Name, Pid]),
            error(suspended_supervisor);

        running ->
            case catch supervisor:which_children(Pid) of
                Res when is_list(Res) ->
                    Res;
                Other ->
                    error_logger:error_msg("release_handler: ~p~nerror during"
                                           " a which_children call to ~p (~w)."
                                           " [State: running] Exiting ... ~n",
                                           [Other, Name, Pid]),
                    error(which_children_failed)
            end
    end.
```

* 首先通过拿到全局所有的应用的master进程
* 拿到master下面的子进程,也就是顶级supervisor
* 获取顶级supervisor下面的所有子进程
* 子进程通过子进程规格定义里的Mods对应:`{Sup, Name, Pid, Mods}`

这样一个流程之后,就获得了一个进程与多个模块的一对多对应关系,我们再看会代码:

```erlang
eval({suspend, Modules}, EvalState) ->
	Procs = get_supervised_procs(),
	NewSuspended =
	lists:foldl(fun(ModSpec, Suspended) ->
		    {Module, Def} = case ModSpec of 
					{Mod, ModTimeout} ->
					    {Mod, ModTimeout};
					Mod ->
					    {Mod, default}
				    end,
		    Timeout = get_opt(suspend_timeout, EvalState, Def),
		    Pids = suspend(Module, Procs, Timeout),
		    [{Module, Pids} | Suspended]
	    end,
	    EvalState#eval_state.suspended,
	    Modules),
	EvalState#eval_state{suspended = NewSuspended};
...
suspend(Mod, Procs, Timeout) ->
    lists:zf(fun({_Sup, _Name, Pid, Mods}) -> 
		     case lists:member(Mod, Mods) of
			 true ->
			     case catch sys_suspend(Pid, Timeout) of
				 ok -> {true, Pid};
				 _ -> 
				     % If the proc hangs, make sure to
				     % resume it when it gets suspended!
				     catch sys:resume(Pid),
				     false
			     end;
			 false ->
			     false
		     end
	     end,
	     Procs).

sys_suspend(Pid, default) ->
    sys:suspend(Pid);
sys_suspend(Pid, Timeout) ->
    sys:suspend(Pid, Timeout).
```
可以看到代码中{pid, Mods}这样的关系,经过一轮转换之后变成了{Module, Pids}的关系,然后并把执行暂停成功的关系保存起来,以后还会用到,这样就完成了suspend指令了.

suspend之后是指令`{load,{rus_gen_server,brutal_purge,brutal_purge}}`,这个指令只是简单的将代码加载进vm而已,继续看下一条指令:`{code_change,up,[{rus_gen_server,[]}]}` 执行`code_change`:

```erlang
eval({code_change, Modules}, EvalState) ->
    eval({code_change, up, Modules}, EvalState);
eval({code_change, Mode, Modules}, EvalState) ->
    Suspended = EvalState#eval_state.suspended,
    Vsns = EvalState#eval_state.vsns,
    Timeout = get_opt(code_change_timeout, EvalState, default),
    lists:foreach(fun({Mod, Extra}) ->
           Vsn =
               case lists:keysearch(Mod, 1, Vsns) of
              {value, {Mod, OldVsn, _NewVsn}}
                when Mode == up -> OldVsn;
              {value, {Mod, _OldVsn, NewVsn}}
                when Mode == down -> {down, NewVsn};
              _ when Mode == up -> undefined;
              _ -> {down, undefined}
               end,
           case lists:keysearch(Mod, 1, Suspended) of
               {value, {_Mod, Pids}} ->
              change_code(Pids, Mod, Vsn, Extra, Timeout);
               _ -> ok
           end
        end,
        Modules),
    EvalState;
...
change_code(Pids, Mod, Vsn, Extra, Timeout) ->
    Fun = fun(Pid) -> 
		  case sys_change_code(Pid, Mod, Vsn, Extra, Timeout) of
		      ok ->
			  ok;
		      {error,Reason} ->
			  throw({code_change_failed,Pid,Mod,Vsn,Reason})
		  end
	  end,
    lists:foreach(Fun, Pids).

sys_change_code(Pid, Mod, Vsn, Extra, default) ->
    sys:change_code(Pid, Mod, Vsn, Extra);
sys_change_code(Pid, Mod, Vsn, Extra, Timeout) ->
    sys:change_code(Pid, Mod, Vsn, Extra, Timeout).
```

code_change指令是拿到了之前保存的已经暂停的进程与模块对应关系列表,然后从中查找去查找模块,然后再逐个进程进行通知执行变更内部状态的方法,完成之后是恢复指令:`{resume,[rus_gen_server]}]`, resume:

```erlang
eval({resume, Modules}, EvalState) ->
    NewSuspended =
	lists:foldl(fun(Mod, Suspended) ->
			    lists:filter(fun({Mod2, Pids}) when Mod2 == Mod ->
						 resume(Pids),
						 false;
					    (_) ->
						 true
					 end,
					 Suspended)
		    end,
		    EvalState#eval_state.suspended,
		    Modules),
    EvalState#eval_state{suspended = NewSuspended};
...
resume(Pids) ->
    lists:foreach(fun(Pid) -> catch sys:resume(Pid) end, Pids).
```

恢复这个指令比较简单,依然是拿到之前的关系列表,然后查找到模块,然后再逐个进程进行恢复,恢复成功之后就从列表里面删除掉这个模块和进程的对应关系.

整个流程就这么简单

-----

# supervisor子规格的dynamic

从上面的代码我们可以看到一个地方:

```erlang
get_procs([{Name, Pid, worker, dynamic} | T], Sup) when is_pid(Pid) -> %% Modules=dynamic
    Mods = maybe_get_dynamic_mods(Name, Pid),  %% 动态去获取Modules
    [{Sup, Name, Pid, Mods} | get_procs(T, Sup)];
get_procs([{Name, Pid, worker, Mods} | T], Sup) when is_pid(Pid), is_list(Mods) -> %% 正常Modules
    [{Sup, Name, Pid, Mods} | get_procs(T, Sup)];
```

了解supervisor子规格定义都知道,最后一个字段是允许设置为dynamic:

```erlang
-type child_spec() :: #{id := child_id(),       % mandatory
         start := mfargs(),      % mandatory
         restart => restart(),   % optional
         shutdown => shutdown(), % optional
         type => worker(),       % optional
         modules => modules()}.
-type modules()  :: [module()] | 'dynamic'.
```

那这个有什么作用呢?

我们之前有用过gen_event吧,没有用过也没有关系,error_logger用过吧,error_logger的就是通过gen_event来实现的,我们知道我们可以通过add_report_handler或者delete_report_handler接口变更回调模块,内部其实使用的是gen_event的add_handler和delete_handler,也就是说gen_event进程的回调模块不是固定的,可以在运行中动态的增加删除,所以挂载到supervisor下的时候,子规格不能填写固定的,只能填dynamic,当然,在升级的时候也是需要动态去获取这个进程的回调模块列表,下面我们来看看代码实现:

```erlang
get_supervised_procs() ->  %% 查找到应用下所有的sup进程
    lists:foldl(
      fun(Application, Procs) ->
              get_master_procs(Application,
                               Procs,
                               application_controller:get_master(Application))
      end,
      [],
      get_application_names()).
...
get_procs([{Name, Pid, worker, dynamic} | T], Sup) when is_pid(Pid) -> %% Modules=dynamic
    Mods = maybe_get_dynamic_mods(Name, Pid),  %% 动态去获取Modules
    [{Sup, Name, Pid, Mods} | get_procs(T, Sup)];
get_procs([{Name, Pid, worker, Mods} | T], Sup) when is_pid(Pid), is_list(Mods) -> %% 正常Modules
    [{Sup, Name, Pid, Mods} | get_procs(T, Sup)];
...
maybe_get_dynamic_mods(Name, Pid) ->
    case catch gen:call(Pid, self(), get_modules) of  %% 通过发送消息获取
        {ok, Res} ->
            Res;
        Other ->
            error_logger:error_msg("release_handler: ~p~nerror during a"
                                   " get_modules call to ~p (~w),"
                                   " there may be an error in it's"
                                   " childspec. Exiting ...~n",
                                   [Other, Name, Pid]),
            error(get_modules_failed)
    end.
```

可以看到上面的代码,当设定为dynamic时,会去发送一个get_modules消息给进程去获取模块列表,我们来看看gen_event代码,应该会有相应接受处理的地方:

```erlang
{_From, Tag, get_modules} ->
	reply(Tag, get_modules(MSL)),
	loop(Parent, ServerName, MSL, Debug, false);
...
%% Message from the release_handler.
%% The list of modules got to be a set, i.e. no duplicate elements!
get_modules(MSL) ->
    Mods = [Handler#handler.module || Handler <- MSL],
    ordsets:to_list(ordsets:from_list(Mods)).
```

可以看到gen_event实现了这个方法,并返回了需要的模块列表;所以大家下次如果在supervisor用到gen_event或者类似的会变更回调模块的进程的时候,记得要填dynamic,不然你的代码更新会失败的.

-----

从这一章中我们了解到,同步指令最后会被转换成调用sys模块的对应执行方法,下一章节我们会从源码的角度看一下sys模块的执行流程.

今天就到这里,玩得开心~

end
