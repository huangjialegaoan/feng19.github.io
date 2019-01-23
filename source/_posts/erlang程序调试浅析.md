---
title: erlang程序调试浅析
date: 2019/1/22 23:10:00
categories:
- other
tags:
- erlang trace
---

程序调试，俗称 debug

一个程序员基本 80% 的时间都在 debug

这是一个很恐怖的比例,我们也不得不去面对

为了减少这个比例,学好调试方法就变得很重要,可以很大程度上提高开发效率

## 调试方法/技巧

### 通用流程

* 定位问题
* 复现问题
* 解决问题

### 调试方法

* 断点调试 - `debugger`
* 打印输出 - `io:format`
* 跟踪 - `trace`

### trace
• **sys** 是一个标准的OTP, 可以允许自 定义trace函数, 记录所有类型的事件等等. 它非常完善且可以很好地用于开发. 但它会稍微影响处于生产 环境的系统, 因为它没有把IO重定向到远程的shell中, 而且他没有限制trace消息的速度。 不过还是推荐阅读其文档模块.

• **dbg** 也是一个标准的OTP. 它的接口在可用性方面显得有点笨拙。但它完全足以满足你所需。问题在于: 你必须要知道你要做什么,因为 dbg可以记录一切， 并在2秒内把系统搞崩溃。

• **tracing BIFs** 作为一个Erang的模块可用。 它们大多作为原始块(the raw blocks)由这个列表中提到的application所调用,但由于他们处于较底层， 比较抽象， 用起来也非常困难。

• **redbug** 是可以在正式的生产 运行系统中使用的trace库， 是eper 的一部分， 它内部有一个速度限制开关， 和一个不错的可用接口。 为了使用它， 你必须把eper的所有依赖项都加上。 这个工具箱非常全面， 你会体验到一次非常有趣的安装。

• **recon_trace** 是recon中负 责trace的模块。 目 的是和redbug有相同的安全水平,但却不要这么多的依赖项。 接口也不一样， 速度限制选项并不完全相同。 它可以只trace指定的函数调用， 没有trace send/recv message （实际在使用OTP的application里面根本没有必要支持trace message这种机制）

> ps: 此段摘自: [Erlang trace总结](https://www.cnblogs.com/zhongwencool/p/erlang_trace.html)

其中`recon_trace`功能丰富,使用门槛底,可以经常使用,以下就以`recon_trace`的使用方法进行深入

## 使用方法

在日常调试中,调试的开始必须要确认跟踪的目标,可以从猜测目标开始进行跟踪,逐步找到出问题的点.

通常其他语言的跟踪都是提供MFA这样的跟踪目标给到大家去确认,但是在erlang的世界里面,还可以增加另外一种纬度来确认目标,就是进程,因为erlang的轻量级进程可以同时运行非常巨大的进程组,因此如果在erlang的时间里面缺少这种纬度的话,调试跟踪是寸步难行的.

试想一下一个服务器里面有10W用户,那么就有10W的进程(至少),当我们想在线上跟踪一个问题的时候,如果只有对方法名的跟踪方式,那么如果这10W用户中有10%在可能会使用到这个方法,那么就是1W个进程,如果在线上直接开启跟踪调试,那么可能瞬间可以刷爆服务器.

```
           _,--------,_      _,--------,_
        ,-'            `-,,-'            `-,
     ,-'              ,-'  '-,              `-,
    |   Matching    -'        '-   Matching    |
    |     Pids     |  Getting   |    Trace     |
    |              |   Traced   |  Patterns    |
    |               -,        ,-               |
     '-,              '-,  ,-'              ,-'
        '-,_          _,-''-,_          _,-'
            '--------'        '--------'
```

`recon_trace:calls/3`定义:

```erlang
-type matchspec()    :: [{[term()], [term()], [term()]}].
-type shellfun()     :: fun((_) -> term()).
-type formatterfun() :: fun((_) -> iodata()).
-type millisecs()    :: non_neg_integer().
-type pidspec()      :: all | existing | new | recon:pid_term().
-type max_traces()   :: non_neg_integer().
-type max_rate()     :: {max_traces(), millisecs()}.

                   %% trace options
-type options()      :: [ {pid, pidspec() | [pidspec(),...]} % default: all
                        | {timestamp, formatter | trace}     % default: formatter
                        | {args, args | arity}               % default: args
                        | {io_server, pid()}                 % default: group_leader()
                        | {formatter, formatterfun()}        % default: internal formatter
                        | return_to | {return_to, boolean()} % default: false
                   %% match pattern options
                        | {scope, global | local}            % default: global
                        ].

-type mod()          :: '_' | module().
-type fn()           :: '_' | atom().
-type args()         :: '_' | 0..255 | return_trace | matchspec() | shellfun().
-type tspec()        :: {mod(), fn(), args()}.
-type max()          :: max_traces() | max_rate().
-type num_matches()  :: non_neg_integer().
-spec calls(tspec() | [tspec(),...], max(), options()) -> num_matches().
```

### 使用前须知

* 模块必须确保已经加载,否则即使设置了跟踪,也是没有效果的
* 指定了方法名的话,如果方法设置了`-inline()`,因为`inline`的机制,实际上已经运行时已经没有方法调用了,因此跟踪无效
* 返回值为数字,表示成功匹配跟踪了多少个条目
* `scope`的选择: `local`表示可以跟踪未导出的方法; `global`表示只跟踪导出的方法

### 示例

目前使用比较多的用法是在服务器端监听客户端是否有发送消息协议,能看到每次数据的传输协议,看到完成协议处理之后的结果返回,能针对单个进程去做,也可以针对一类进程去做

另外也可以将结果保存成文本,方便分析问题,相比于添加打印代码的调试方式,可以说这种方式是有极大的便捷性,因为不需要在添加代码,然后再编译加载新代码才能使用,这种方法只需要在命令行界面输入,即可使用

示例是模拟客户端发送消息给服务器,服务器查看是否收到,查看传输的数据是否正常,处理的结果是否正常,等等

#### 应用启动`trace_example_app`:

```erlang
start(_StartType, _StartArgs) ->
    % 使用ranch开启tcp端口
    ranch:start_listener(tcp, ranch_tcp, #{connection_type => worker, socket_opts => [{port, 0}]}, te_tcp_handler, []),
    trace_example_sup:start_link().
```

#### 服务器`socket`进程模块`te_tcp_handler`:

```erlang
-module(te_tcp_handler).
-behaviour(ranch_protocol).
-behaviour(gen_server).

%% 处理tcp消息
handle_info({tcp, Socket, ReqBinary}, State) ->
    case dispatch_msg(ReqBinary, State) of
        {ok, RespBinList, NewState} ->
            send(RespBinList, Socket),
            ?SET_ACTIVE_ONCE(Socket),
            {noreply, NewState};
        {ok, NewState} ->
            ?SET_ACTIVE_ONCE(Socket),
            {noreply, NewState};
        {stop, Reason, RespBinList, NewState} ->
            io:format("net_handler stop err:~p", [Reason]),
            send(RespBinList, Socket),
            ?SET_ACTIVE_ONCE(Socket),
            {stop, normal, NewState};
        {stop, Reason, NewState} ->
            io:format("net_handler stop err:~p", [Reason]),
            ?SET_ACTIVE_ONCE(Socket),
            {stop, normal, NewState}
    end;
....

%% 派遣消息
dispatch_msg(ReqBinary, State) ->
    Term = jsx:decode(ReqBinary, [{labels, atom}, return_maps]),
    case maps:find(event, Term) of
        {ok, Event} ->
            {ok, Data} = maps:find(data, Term),
            dispatch_msg(binary_to_atom(Event, utf8), Data, State);
        _ ->
            {ok, State}
    end.

%% 派遣消息
dispatch_msg(Event, Data, State) ->
    apply(fun ?MODULE:Event/2, [Data, State]).

%% 发送消息
send(RespBinList, Socket) when is_list(RespBinList) ->
    lists:foreach(
        fun(Binary) ->
            gen_tcp:send(Socket, Binary)
        end, RespBinList);
send(Binary, Socket) ->
    gen_tcp:send(Socket, Binary).

%% 处理登录消息
login(#{account := Account}, State) ->
    Name = lists:concat([binary_to_list(Account), "_server"]),
    register(list_to_atom(Name), self()),
    {ok, State#state{account = Account}}.

%% 处理心跳消息
heartbeat(_Data, State) ->
    {ok, State#state{last_heartbeat_time = erlang:system_time(second)}}.

%% 处理进入房间消息
enter_room(#{room_type := RoomType}, State) ->
    {ok, State#state{room_type = RoomType}}.

%% 处理离开房间消息
leave_room(_Data, State) ->
    {ok, State#state{room_type = undefined}}.
```

#### 使用示例`trace_example`:

```erlang
trace(first) ->
    code:ensure_loaded(te_tcp_handler),
    %% 查看客户端发送的数据
    NumMatches = recon_trace:calls({te_tcp_handler, dispatch_msg, '_'}, 3, [{scope, local}]),
    io:format("~nNumMatches:~p~n", [NumMatches]),
    te_client:start(jake_ma_1),
    timer:sleep(3000),
    te_client:stop(jake_ma_1),
    recon_trace:clear();
trace(arity) ->
    %% 指定参数数量
    recon_trace:calls({te_tcp_handler, dispatch_msg, 3}, 3, [{scope, local}]),
    te_client:start(jake_ma_1),
    timer:sleep(3000),
    te_client:stop(jake_ma_1),
    recon_trace:clear();
trace(args) ->
    %% 参数列表
    MatchSpec = ets:fun2ms(fun({'_', '_', '_'}) -> ok end),
    recon_trace:calls({te_tcp_handler, dispatch_msg, MatchSpec}, 3, [{scope, local}]),
    te_client:start(jake_ma_1),
    te_client:enter_room(jake_ma_1, 1),
    timer:sleep(3000),
    te_client:stop(jake_ma_1),
    recon_trace:clear();
trace(args2) ->
    %% 参数列表
    MatchSpec = ets:fun2ms(fun({enter_room, '_', '_'}) -> ok end),
    recon_trace:calls({te_tcp_handler, dispatch_msg, MatchSpec}, 3, [{scope, local}]),
    te_client:start(jake_ma_1),
    te_client:enter_room(jake_ma_1, 1),
    timer:sleep(3000),
    te_client:stop(jake_ma_1),
    recon_trace:clear();
trace(filter_heartbeat) ->
    %% 排除心跳协议
    MatchSpec = ets:fun2ms(fun({Event, '_', '_'}) when Event =/= heartbeat -> ok end),
    recon_trace:calls({te_tcp_handler, dispatch_msg, MatchSpec}, 3, [{scope, local}]),
    te_client:start(jake_ma_1),
    te_client:enter_room(jake_ma_1, 1),
    te_client:leave_room(jake_ma_1),
    timer:sleep(3000),
    te_client:stop(jake_ma_1),
    recon_trace:clear();
trace(return_trace) ->
    %% 跟踪返回值
    MatchSpec = ets:fun2ms(fun({Event, '_', '_'}) when Event =/= heartbeat -> return_trace() end),
    recon_trace:calls({te_tcp_handler, dispatch_msg, MatchSpec}, 10, [{scope, local}]),
    te_client:start(jake_ma_1),
    te_client:enter_room(jake_ma_1, 1),
    te_client:leave_room(jake_ma_1),
    timer:sleep(3000),
    te_client:stop(jake_ma_1),
    recon_trace:clear();
trace(time) ->
    %% 频率限制,每秒最多2个,超过这个频率会停掉跟踪,没有超过则继续
    MatchSpec = ets:fun2ms(fun({Event, '_', '_'}) when Event =/= heartbeat -> ok end),
    recon_trace:calls({te_tcp_handler, dispatch_msg, MatchSpec}, {2, 1000}, [{scope, local}]),
    te_client:start(jake_ma_1),
    te_client:enter_room(jake_ma_1, 1),
    timer:sleep(1000),
    te_client:leave_room(jake_ma_1),
    te_client:enter_room(jake_ma_1, 1),
    timer:sleep(1000),
    te_client:leave_room(jake_ma_1),
    timer:sleep(1000),
    te_client:stop(jake_ma_1),
    recon_trace:clear();
trace(time2) ->
    %% 频率限制,每秒最多2个,超过这个频率会停掉跟踪,没有超过则继续
    MatchSpec = ets:fun2ms(fun({Event, '_', '_'}) when Event =/= heartbeat -> ok end),
    recon_trace:calls({te_tcp_handler, dispatch_msg, MatchSpec}, {2, 1000}, [{scope, local}]),
    te_client:start(jake_ma_1),
    te_client:enter_room(jake_ma_1, 1),
    te_client:leave_room(jake_ma_1),
    te_client:enter_room(jake_ma_1, 1),
    te_client:leave_room(jake_ma_1),
    timer:sleep(3000),
    te_client:stop(jake_ma_1),
    recon_trace:clear();
trace(to_file) ->
    %% 结果输出到文件
    MatchSpec = ets:fun2ms(fun({Event, '_', '_'}) when Event =/= heartbeat -> ok end),
    {ok, IO} = file:open("trace_output.txt", [write]),
    recon_trace:calls({te_tcp_handler, dispatch_msg, MatchSpec}, 10, [{scope, local}, {io_server, IO}]),
    te_client:start(jake_ma_1),
    te_client:enter_room(jake_ma_1, 1),
    te_client:leave_room(jake_ma_1),
    te_client:enter_room(jake_ma_1, 1),
    te_client:leave_room(jake_ma_1),
    timer:sleep(3000),
    file:close(IO),
    te_client:stop(jake_ma_1),
    recon_trace:clear();
trace(record_print) ->
    %% record数据格式化打印
    MatchSpec = ets:fun2ms(fun({Event, '_', '_'}) when Event =/= heartbeat -> return_trace() end),
    recon_rec:import(te_tcp_handler),
    recon_trace:calls({te_tcp_handler, dispatch_msg, MatchSpec}, 10, [{scope, local}]),
    te_client:start(jake_ma_1),
    te_client:enter_room(jake_ma_1, 1),
    te_client:leave_room(jake_ma_1),
    timer:sleep(3000),
    recon_rec:clear(),
    te_client:stop(jake_ma_1),
    recon_trace:clear();
trace(record_print2) ->
    %% record数据格式化打印, 只显示指定字段
    MatchSpec = ets:fun2ms(fun({Event, '_', '_'}) when Event =/= heartbeat -> return_trace() end),
    recon_rec:import(te_tcp_handler),
    recon_rec:limit(state, 4, [room_type]),
    recon_trace:calls({te_tcp_handler, dispatch_msg, MatchSpec}, 10, [{scope, local}]),
    te_client:start(jake_ma_1),
    te_client:enter_room(jake_ma_1, 1),
    te_client:leave_room(jake_ma_1),
    timer:sleep(3000),
    recon_rec:clear(),
    te_client:stop(jake_ma_1),
    recon_trace:clear();
trace(all_pid) ->
    %% 多个进程
    MatchSpec = ets:fun2ms(fun({Event, '_', '_'}) when Event =/= heartbeat -> ok end),
    recon_rec:import(te_tcp_handler),
    recon_trace:calls({te_tcp_handler, dispatch_msg, MatchSpec}, 30, [{scope, local}]),
    te_client:loop_start(1000),
    te_client:enter_room(jake_ma_1, 1),
    te_client:leave_room(jake_ma_1),
    timer:sleep(3000),
    recon_rec:clear(),
    te_client:loop_stop(1000),
    recon_trace:clear();
trace(new_pid) ->
    %% 多个进程
    MatchSpec = ets:fun2ms(fun({Event, '_', '_'}) when Event =/= heartbeat -> ok end),
    recon_rec:import(te_tcp_handler),
    te_client:loop_start(1000),
    recon_trace:calls({te_tcp_handler, dispatch_msg, MatchSpec}, 30, [{scope, local}, {pid, new}]),
    te_client:enter_room(jake_ma_1, 1),
    te_client:leave_room(jake_ma_1),
    timer:sleep(3000),
    recon_rec:clear(),
    te_client:loop_stop(1000),
    recon_trace:clear();
trace(new_pid2) ->
    %% 多个进程
    MatchSpec = ets:fun2ms(fun({Event, '_', '_'}) when Event =/= heartbeat -> ok end),
    recon_rec:import(te_tcp_handler),
    te_client:loop_start(1000),
    recon_trace:calls({te_tcp_handler, dispatch_msg, MatchSpec}, 30, [{scope, local}, {pid, new}]),
    te_client:start(jake_ma_1001),
    te_client:enter_room(jake_ma_1001, 1),
    te_client:leave_room(jake_ma_1001),
    timer:sleep(3000),
    recon_rec:clear(),
    te_client:loop_stop(1001),
    recon_trace:clear();
trace(pid) ->
    %% 指定进程
    MatchSpec = ets:fun2ms(fun({Event, '_', '_'}) when Event =/= heartbeat -> ok end),
    recon_rec:import(te_tcp_handler),
    te_client:loop_start(1000),
    te_client:start(jake_ma_1001),
    Pid = wait_name(jake_ma_1001_server),
    recon_trace:calls({te_tcp_handler, dispatch_msg, MatchSpec}, 30, [{scope, local}, {pid, Pid}]),
    te_client:enter_room(jake_ma_1001, 1),
    te_client:leave_room(jake_ma_1001),
    timer:sleep(3000),
    recon_rec:clear(),
    te_client:loop_stop(1001),
    recon_trace:clear().
```

### 应用拓展

- 经常使用的定义可以封装成方法加入到`user_default`模块里面

## 结论

我们试着将一个程序系统跟人体系统进行类比,可以发现:

程序出bug =>  人生病

找bug(调试) => 诊断病因

我们知道中医诊断方法,四诊: **望闻问切**

* **望**: 观察病人的身体状况，包括面色、舌苔等;
* **闻**: 听病人的说话、咳嗽、喘息，并嗅其口中或身上是否有异味;
* **问**: 询问病人症状，以及患病史等;
* **切**: 用手把脉或按腹部诊察是否有异常

以上都是通过使用不同的手段去对人体系统进行信息了解,

只有当了解的信息能匹配到具体的病状,才能诊断出是什么问题.

人可能生不同的病,种类非常的多,现代医学越来越发达,开发出了越来越多的诊断仪器来帮助医生找出病因

我们开发的系统也是一样,问题的种类同样有非常多种,并不是所有的问题都能用一种手段就能诊断出问题,**因此多掌握一种trace方法能让调试有更多的选择**

最后我们再来讲一个跟医学有关的小故事:

### 扁鹊三兄弟的故事

>根据典记
>魏文王曾求教于名医扁鹊：“你们家兄弟三人，都精于医术，谁是医术最好的呢？”
>扁鹊：“大哥最好，二哥差些，我是三人中最差的一个。”
>魏王不解地说：“请你介绍的详细些。” 
>扁鹊解释说：“
>大哥治病，是在病情发作之前，那时候病人自己还不觉得有病，但大哥就下药铲除了病根，使他的医术难以被人认可，所以没有名气，只是在我们家中被推崇备至。
>
>我的二哥治病，是在病初起之时，症状尚不十分明显，病人也没有觉得痛苦，二哥就能药到病除，使乡里人都认为二哥只是治小病很灵。
>
>我治病，都是在病情十分严重之时，病人痛苦万分，病人家属心急如焚。此时，他们看到我在经脉上穿刺，用针放血，或在患处敷以毒药以毒攻毒，或动大手术直指病灶，使重病人病情得到缓解或很快治愈，所以我名闻天下。”
>
>魏王大悟。

### 故事结论

> 解决患难者强，防患于未然者神

- 我们debug的也很像看病,也可以分为三个阶段进行
- 防范 => 编码规范 
- 治前 => 代码测试
- 治中 => 调试方法

### 防范

>   [代码规范 erlang_guidelines](https://github.com/feng19/erlang_guidelines)

* 代码可读性 
* 模块区分
* 方法功能区分
* ...

### 治前

* 单元测试 

  > Erlang趣学指南:24章 - Eunit: 单元测试框架

* 集成测试 

  > Erlang趣学指南:28章 - 不寻常的Common Test

### 治中

* 调试方法



在处理完bug之后,可以多思考如果防范于未然,避免类似的问题再出现

**这里并没有结束,只是新的开始**



## 参考连接

* [工作中常见的程序调试方法](https://zhuanlan.zhihu.com/p/26824111)
* [Erlang trace总结](https://www.cnblogs.com/zhongwencool/p/erlang_trace.html)
* [Trace Tool Builder](http://erlang.org/doc/apps/observer/ttb_ug.html)
* [recon_trace](http://ferd.github.io/recon/recon_trace.html)
* [Learn you some erlang](https://learnyousomeerlang.com/content)
* [dbg](http://www.erlang-factory.com/upload/presentations/316/dbg[1].pdf)