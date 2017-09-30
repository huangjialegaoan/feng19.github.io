---
title: Erlang 发布版本升级-5 supervisor更新说明
date: 2017/10/1 0:0:0
categories:
- Erlang-Release-Upgrade
tags:
- erlang
- release upgrade
---

supervisor在我们热更中起到至关重要,而且设计到的内容比较多,需要注意的事项也很多,所以这里特别开了一个章节来说.

------

# 指令区别

上一章我们说道同步热更指令有两种:

```erlang
{update, Module, {advanced, Extra}}  %% gen_server类指令
{update, Module, supervisor}  %% supervisor专用指令
```

同样是更新进程内部状态,但是为什么要分开两个指令来区分呢?

单从这里,看不出区别来,因此我决定去看看源码实现:

之前我们知道普通指令会在relup阶段转成底层指令然后放入relup文件,然后我找到了这一段,位置在systools_rc:expand_script/1

```erlang
{update, Mod, Change} when is_tuple(Change) -> %% gen_server的指令
    {update, Mod, Change, brutal_purge, brutal_purge, []};
{update, Mod, supervisor} -> %% supervisor的指令
    {update, Mod, static, default, {advanced,[]}, brutal_purge, brutal_purge, []};
```

我们发现{update, Mod, Change}被转成了:

```erlang
{update, Mod, Change, brutal_purge, brutal_purge, []};
```

继续跟踪,上面这条指令在内部还会转换一次,最后是:

```erlang
{update, Mod, dynamic, default, Change, brutal_purge, brutal_purge, []};
```

而我们的{update, Mod, supervisor}被转成了:

```erlang
{update, Mod, static, default, {advanced,[]}, brutal_purge, brutal_purge, []};
```

可以看到第三个字段明显不同gen_server的指令的是dynamic, 而supervisor的指令是static:

```erlang
{update, Mod, dynamic, default, Change, brutal_purge, brutal_purge, []};
{update, Mod, static, default, {advanced,[]}, brutal_purge, brutal_purge, []};
```

我们再结合文档来看看:

High-Level Instructions

```erlang
{update, Mod}
{update, Mod, supervisor}
{update, Mod, Change}
{update, Mod, DepMods}
{update, Mod, Change, DepMods}
{update, Mod, Change, PrePurge, PostPurge, DepMods}
{update, Mod, Timeout, Change, PrePurge, PostPurge, DepMods}
{update, Mod, ModType, Timeout, Change, PrePurge, PostPurge, DepMods}
  Mod = atom()
  ModType = static | dynamic
  Timeout = int()>0 | default | infinity
  Change = soft | {advanced,Extra}
    Extra = term()
  PrePurge = PostPurge = soft_purge | brutal_purge
  DepMods = [Mod]
```

第三个字段的定义为ModType,继续看文档关于ModType的定义解析:

> Defaults to dynamic. It specifies if the code is "dynamic", that is, if a process using the module spontaneously switches to new code, or if it is "static". When doing an advanced update and upgrade, the new version of a dynamic module is loaded before the process is asked to change code. When downgrading, the process is asked to change code before loading the new version. For static modules, the new version is loaded before the process is asked to change code, both in the case of upgrading and downgrading. Callback modules are dynamic.
> 默认指定的是dynamic,当进行升级的时候,新版本模块会在执行change code前加载;当进行降级的时候,会先执行change code之后在加载新版本模块
> 而如果指定static,则升级和降级的时候都是先加载新版本模块再执行change code操作

这里说的是,更新的顺序不同,

指定dynamic的时候:

1. 升级的时候,系统先加载模块代码,然后再执行回调方法change code,让进程更新状态
2. 降级的时候,系统会先执行回调方法change code,然后再加载模块代码,让进程更新状态

指定static的时候:

升级和降级都同样是先加载模块代码,然后再执行回调方法change code,让进程更新状态.

为什么otp要这么设定呢?从这方面我们还是看不出来,接下来我们看看supervisor的更新过程:

------

# supervisor的更新过程

有看过supervisor的源码的同学可能很少会注意到,其实supervisor的behaviour是gen_server:

```erlang
-module(supervisor).

-behaviour(gen_server).

%% External exports
-export([start_link/2, start_link/3,
	 start_child/2, restart_child/2,
	 delete_child/2, terminate_child/2,
	 which_children/1, count_children/1,
	 check_childspecs/1, get_childspec/2]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).
-export([try_again_restart/2]).
```

因此supervisor同样也实现了code_change回调:

```erlang

code_change(_, State, _) ->
    case (State#state.module):init(State#state.args) of
	{ok, {SupFlags, StartSpec}} ->
	    case set_flags(SupFlags, State) of
		{ok, State1}  ->
                    update_childspec(State1, StartSpec);
		{invalid_type, SupFlags} ->
		    {error, {bad_flags, SupFlags}}; % backwards compatibility
		Error ->
		    {error, Error}
	    end;
	ignore ->
	    {ok, State};
	Error ->
	    Error
    end.
```

发现当supervisor进程执行code_change回调方法只是简单的执行回调方法Mod:init/1,原来这个init/1并不仅仅是supervisor进程启动的时候才会调用,还会在更新的时候调用.

结合之前的文档我们得到如下的结论:

我们都知道,sup模块的init/1返回的是supervisor的规格定义,而sup模块更新最主要的是更新supervisor内部的state,所以升级的时候必须要先加载新的模块代码,然后才能获得新的规格定义;相反降级的时候必要要先加载旧的代码,然后才能获得旧的规格定义.

解决了一个问题之后,我们也得到了一个更新supervisor进程的注意事项:

sup模块的 init/1 并不仅仅是supervisor进程启动的时候才会调用,还会在更新的时候调用,因此不要在init/1里面执行时间比较长的操作,也不要在里面做一些影响系统状态的操作,只要返回sup的规格定义就行了.

除了这个注意事项,还有其它的~

------

# 注意事项

- 更新sup模块的规格定义并不会影响到现有的子进程.

- 新增的子规格会被加到进去sup的state但是不会自动启动.

- 删除的子规格不会被删除,也不会自动关闭.

- 如果要实现上述两个功能的,须手动增建apply指令.如下:

  ```erlang
  %% 假定我们要新增一个m1模块到ch_sup:
  {"2",
   [{"1",
     [{update, ch_sup, supervisor},
      {apply, {supervisor, restart_child, [ch_sup, m1]}}
     ]}],
   [{"1",
     [{apply, {supervisor, terminate_child, [ch_sup, m1]}},
      {apply, {supervisor, delete_child, [ch_sup, m1]}},
      {update, ch_sup, supervisor}
     ]}]
  }.
  ```


- 我们可以看到上面的列子,指令之间的顺序很重要.

> 如果在版本1升级到2时,在`{update, ch_sup, supervisor}`先执行`{apply, {supervisor, restart_child, [ch_sup, m1]}}`是不会成功的,
> 因为此时ch_sup里并没有m1这个模块的规格定义; 
> 同理,在降级的时候,如果`{apply, {supervisor, terminate_child, [ch_sup, m1]}}`,
> `{apply, {supervisor, delete_child, [ch_sup, m1]}}`在`{update, ch_sup, supervisor}`后面,也不会成功,
> 因为ch_sup还原之后就没有了m1这个模块的规格定义了.

- 其实用之前我们说的appup插件的话,并不需要手动操作上面这一步,插件会在生成阶段动态的去调用前后两个版本sup的init方法,然后通过比对规格定义得到这些指令.
- 此外,执行apply指令的时候,我们的ch_sup进程的必须有注册名,我们知道pid是动态的,也不能在appup文件里面知道ch_sup的pid,因此必须有注册名,才能查找到ch_sup这个进程.

------

很多同学可能会有一个疑问,我们到底是升级代码还是升级进程?可能有时候会两个概念混淆了,所以下一章,我们会来看看:升级模块与进程.

今天就到这里,玩得开心~

end
