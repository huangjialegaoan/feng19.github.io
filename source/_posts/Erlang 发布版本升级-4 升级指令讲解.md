---
title: Erlang 发布版本升级-4 升级指令讲解
date: 2017/10/1 0:0:0
categories:
- Erlang-Release-Upgrade
tags:
- erlang
- release upgrade
---

插件虽然能帮助我们很方便的生成appup文件,但是如果你想要在升级的时候执行一下你的升级mnesia的record数据结构方法,插件不能分析出来我们要做这个的.

所以插件只能帮我们做其中一部分工作而已,另外一部分需要我们自己去处理,因此我们也需要对appup里面的指令进行进一步的学习.

------

# 预备知识

先来看看官方对于appup文件的描述:

> The application upgrade file defines how an application is upgraded or downgraded in a running system.
> This file is used by the functions in [systools](http://erlang.org/doc/man/systools.html) when generating a release upgrade file relup.
> .appup文件是定义运行中的系统的应用怎么升级和降级,文件后期会被转成.relup 文件

之前我们也看过appup文件的内容了,大概是长这个样子:

```erlang
%% appup generated for rus_rel by rebar3_appup_plugin (2017/09/18 14:46:53)
{ "0.2.0",
    [{ "0.1.0",
        [{update,rus_gen_server,{advanced,[]},brutal_purge,brutal_purge,[]}] }],
    [{ "0.1.0",
        [{update,rus_gen_server,{advanced,[]},brutal_purge,brutal_purge,[]}] }]
}.
```

现在再来看看它的格式定义:

```erlang
{Vsn,
  [{UpFromVsn, Instructions}, ...],
  [{DownToVsn, Instructions}, ...]}.
```

> Vsn = string()
> Current application version.
> UpFromVsn = string() | binary()
> An earlier application version to upgrade from. If it is a string, it is interpreted as a specific version number. If it is a binary, it is interpreted as a regular expression that can match multiple version numbers.
> DownToVsn = string() | binary()
> An earlier application version to downgrade to. If it is a string, it is interpreted as a specific version number. If it is a binary, it is interpreted as a regular expression that can match multiple version numbers.
> Instructions
> A list of release upgrade instructions, see [Release Upgrade Instructions](http://erlang.org/doc/man/appup.html#Release%20Upgrade%20Instructions). It is recommended to use high-level instructions only. These are automatically translated to low-level instructions by systools when creating the relup file.

```erlang
{Vsn, %% Vsn 表示的是新版本
  [{UpFromVsn, Instructions}, ...], %% 第二个属性表示升级
  [{DownToVsn, Instructions}, ...]  %% 第三个表示降级
}.
```

UpFromVsn 表示 从 UpFromVsn 这个版本升级到 Vsn 需要做的指令(Instructions), 可以看到这是一个列表,表示可以支持多个版本

DownToVsn 表示 从 Vsn 这个版本降级到 DownToVsn 需要做的指令(Instructions), 可以看到这也是一个列表,表示可以支持多个版本

下面我们来看一下例子:

```erlang
{"2.0.0",
 [{"1.0.0", [{load_module, m}]}], %% 版本"1.0.0"升级到版本"2.0.0", 加载模块m
 [{"1.0.0", [{load_module, m}]}] %% 版本"2.0.0"降级到版本"1.0.0", 加载模块m
}.
```

从文档中可以看到如果UpFromVsn或者DownToVsn的值是binary类型的话,表示可以用正则表达式去匹配版本,可以为多个版本:

```erlang
{"2.0.0",
 [{<<"1\\.[0-9]+\\.[0-9]+">>, [{load_module, m}]}],  %% 所有版本"1.*.*"升级到版本"2.0.0", 加载模块m
 [{<<"1\\.[0-9]+\\.[0-9]+">>, [{load_module, m}]}]   %% 版本"2.0.0"降级到所有版本"1.*.*", 加载模块m
}.
```

另外比较重要的一点是, [官方文档](http://erlang.org/doc/design_principles/release_handling.html)提到:

> It is thus recommended that code is changed in as small steps as possible, and always kept backwards compatible. 官方推荐每次更新版本尽可能使用每个小改动版本,用多个版本迭代更新来进行,并且保持向后兼容,利于版本回退.

下面回到正题,来说说指令(Instructions)

------

# 指令(Instructions)

> OTP supports a set of release handling instructions that are used when creating .appup files. The release handler understands a subset of these, the low-levelinstructions. To make it easier for the user, there are also a number of high-levelinstructions, which are translated to low-level instructions by systools:make_relup.
> Some of the most frequently used instructions are described in this section. The complete list of instructions is included in the appup(4) manual page in SASL.

指令分为两种,普通(High-Level)指令和底层(Low-Level)指令的区别: 普通指令是提供给用户使用的,而底层指令是通过普通指令转换而来的.

我们之前生产的*.appup文件, 会在版本发布前的relup阶段,通过调用ystools:make_relup将全部普通指令转换为底层指令.

这个章节我们只是介绍了一些比较常用到的指令,另外还有很多指令大家可以去这个[连接](http://erlang.org/doc/man/appup.html)获取更加详细的文档.

1. 简单代码热更指令:

   ```erlang
   {load_module, Module}
   ```

   这个指令的作用就跟我们之前在eshell里面执行l(Module).一样


2. 同步代码热更指令:

   ```erlang
   {update, Module, {advanced, Extra}}
   {update, Module, supervisor}
   ```

   可以看到这里有两个:第一个是更新进程内部状态的,而第二个是专门提供给supervisor跟新sup进程内部状态的;之后我会说一下他们的区别.


3. 应用(application)指令:

- {add_application, app} - 增加app应用-根据relup自动生成
- {remove_application, app} - 删除app应用-根据relup自动生成
- {restart_application, app} - 重启app应用

4. 启动关闭进程指令:

   ```erlang
   {start, [Mod]}  %% 启动
     Mod = atom()
   ```

   内部通过执行方法supervisor:restart_child/2,启动所有使用Mod的进程.

   ```erlang
   {stop, [Mod]}
     Mod = atom()
   ```

   内部通过执行方法supervisor:terminate_child/2,关闭所有使用Mod的进程.


5. 执行指令:

   ```erlang
   {apply, {M, F, A}}
     M = F = atom()
     A = [term()]
   ```

   内部实现就是apply(M, F, A).


6. 更新配置:

   这个并不需要指令,但是我们的配置发生变化了怎么办呢?

   文档告诉我们:

   * Module is the application callback module as defined by the mod key in the .appfile.
   * Changed and New are lists of {Par,Val} for all changed and added configuration parameters, respectively.
   * Removed is a list of all parameters Par that have been removed.

   ```erlang
   %% 只要在应用回调模块实现config_change/3方法,在更新的时候,otp就会回调这个方法
   Module:config_change(Changed, New, Removed).
   %% Changed-有修改的配置 和 New-新增的配置 都是键值对列表[{Par,Val},...]
   %% Removed-删除的配置 是所有键的列表[Par,...]
   ```


7. 非erlang代码的更新:

   官方并没有支持,但是给了一个解决方案,就是在erlang的code_change方法里先关闭旧的port然后再开启新的port.

------

这一章简单的介绍了一下各个指令的说明,看过官方文档的同学可能知道,supervisor进程更新方面官方文档上有很长的一段描述,所以我特此新开了一章关于supervisor进程的更新说明.

今天就这样,玩得开心~

end
