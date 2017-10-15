---
title: Erlang 发布版本升级-3 升级发布版本工具讲解
date: 2017/10/1 0:0:3
categories:
- Erlang-Release-Upgrade
tags:
- erlang
- release upgrade
---

为了不加班,所以程序员一般都会开发一些便捷的工具来提高开发效率,下面就让我来给大家讲讲升级发布版本工具:[rebar3_appup_plugin](https://github.com/lrascao/rebar3_appup_plugin)

------

# 发布

看前缀就知道,这是一个rebar3的插件,如果对rebar3不熟悉的请先移步去[官网](https://www.rebar3.org/)看看文档.

那我们先来创建一个rebar3的项目:

```erlang
rebar3 new release rus_rel && cd rus_rel
```

接下来,当然是添加这个插件到rebar.confg里面:

```erlang
{erl_opts, [debug_info]}.
{plugins, [
    rebar3_appup_plugin
]}.
{deps, []}.

{relx, [
    {release, { rus_rel, "0.1.0" }, [rus_rel,sasl]},
    {sys_config, "./config/sys.config"},
    {vm_args, "./config/vm.args"},
    {dev_mode, false},
    {include_erts, false},
    {extended_start_script, true}
]}.
```

记得relx的配置dev_mode要修改为false(此处是演示,正式设置的时候当然是设置profiles里面的设置)

我们需要增加一个gen_server模块

rus_gen_server.erl :

```erlang
-module(rus_gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
    terminate/2, code_change/3]).
-export([
    start_link/0
]).
-record(state, {}).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) ->
    {ok, #state{}}.
handle_cast(_Info, State) ->
    {noreply, State}.
handle_call(_Info, _From, State) ->
    {reply, ok, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, State) ->
    {ok, State}.

code_change(OldVsn, State, Extra) ->
    io:format("~p ~p ~p~n", [OldVsn, State, Extra]),
    {ok, State}.

```

然后我们在sup里面增加这个模块作为子进程,之后我们会在第二版修改这个进程的内部状态

rus_rel_sup.erl :

```erlang
-module(rus_rel_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).
-define(SERVER, ?MODULE).
%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).
%%====================================================================
%% Supervisor callbacks
%%====================================================================
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Children = [
        {rus_gen_server, {rus_gen_server, start_link, []}, permanent, 5000, worker, [rus_gen_server]}
    ],
    {ok, { {one_for_all, 0, 1}, Children} }.
```

然后我们来发布一个版本:

```erlang
rebar3 release tar
```

就这么简单,我们0.1.0的版本就发布好了,接下来运行起来:

```erlang
$ tar zxf rus_rel-0.1.0.tar.gz // 解压
$ ./bin/rus start // 运行
$ ./bin/rus_rel versions
Installed versions:
* 0.1.0	permanent
```

ok, 准备工作完成

------

# 发布版本升级

接下来我们修改gen_server模块state的数据结构,增加一个字段id

```erlang
-module(rus_gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
    terminate/2, code_change/3]).
-export([
    start_link/0
]).
-record(state, {id}).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) ->
    {ok, #state{}}.
handle_cast(_Info, State) ->
    {noreply, State}.
handle_call(_Info, _From, State) ->
    {reply, ok, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, State) ->
    {ok, State}.

code_change(OldVsn, {state}, Extra) ->
    io:format("~p ~p ~p~n", [OldVsn, State, Extra]),
    {ok, #state{id = 1}}.
```

然后别忘记了修改版本号

rus_rel.app.src修改vsn为0.2.0

rebar.config的{ rus_rel, "0.1.0" }修改为{ rus_rel, "0.2.0" }

然后先发布一下:

```erlang
rebar3 release
```

之后再运行插件:

```erlang
rebar3 appup generate
```

这个插件需要之前的版本也在本地目录下面,因为它的运行机制是通过对比两个rel文件夹的beam, 然后根据各自不同自动生成.appup 文件:

```erlang
%% appup generated for rus_rel by rebar3_appup_plugin (2017/09/18 14:46:53)
{ "0.2.0",
    [{ "0.1.0",
        [{update,rus_gen_server,{advanced,[]},brutal_purge,brutal_purge,[]}] }],
    [{ "0.1.0",
        [{update,rus_gen_server,{advanced,[]},brutal_purge,brutal_purge,[]}] }]
}.
```

然后我们再执行relup 和 打包:

```erlang
rebar3 relup tar
```

这样我们就获得了一个带有更新指令的发布,然后将这个发布包发到运行根目录下的releases文件夹下面, 并执行更新命令:

```erlang
$ ./bin/rus_rel upgrade 0.2.0
Release 0.2.0 not found, attempting to unpack releases/rus_rel-0.2.0.tar.gz
Unpacked successfully: "0.2.0"
Installed Release: 0.2.0
Made release permanent: "0.2.0"
$ ./bin/rus_rel versions
Installed versions:
* 0.2.0	permanent
* 0.1.0	old
```

ok,整个更新就完成了,你可以attach进去看一下进程的内部状态,发现已经更新了.

------

# appup插件说明

支持功能:

- 原理:通过对比两个rel文件夹的beam, 然后根据各自不同自动生成.appup 文件.
- 支持.appup.src 生成.appup 文件.
- 当gen_server 存在了-state_record(state),插件会自动注入state变更代码到beam里, [More info](https://github.com/lrascao/rebar3_appup_plugin/blob/develop/doc/UPGRADE_DOWNGRADE.md).
- 支持根据两个版本sup模块init的返回的规格定义不同,生成相应的升级指令
- 自动生成模块之间的依赖关系.

使用注意事项:

- 两个版本必须编译参数里必须都有debug_info ,因为插件需要debug_info 的信息, 担心源码安全的话, 可以通过在项目下增加.erlang.crypt 文件来保护源码.
- 如果需要手动修改.appup 文件, 则需要编写.appup.src , 但是.appup.src 生成.appup 文件后,插件不会再生成.appup文件,因此建议先用rebar3 appup generate 出 .appup文件,然后再以这个文件的基础去修改,然后作为.appup.src.

------

从本章节看到发布的升级依赖于appup文件里的指令进行的,下一章节我们来详细讲讲升级指令.

今天就到这里,玩得开心~~~

end
