---
title: Erlang 发布持续集成流程-1
date: 2017/10/14 16:30:0
categories:
- Erlang-Release-CI
tags:
- erlang
- release upgrade
- CI
---

# 前言

## 持续集成介绍

我比较懒,这里我就不介绍了,直接贴连接:[持续集成是什么？](http://www.ruanyifeng.com/blog/2015/09/continuous-integration.html)

## 流程

公司的发布流程分为四个阶段:`DEV->SIT->UAT->PROD` ,这也代表着,我们的持续集成(`CI`)也分同样的四个阶段.

`DEV`和`SIT`都是在内网,可以通过`ssh`直连,但是`UAT`和`PROD`在外网,开发不能直接更新,只能通过丢`relup`的`release`包给到运维那边去处理

因此现在如果要做`CI`的话,前两个环境通过`ssh`直接操作,后两个只能是打包了

公司用的是`gitlab`的`CI`,所以下面我们来看看如何用`gitlab`的`CI`去完成`erlang`发布应用的持续集成.

# DEV环境

## 大致步骤

* 编译阶段,通过则进入下一步,不通过发送通知
* 测试阶段,通过则进入下一步,不通过发送通知
* 部署阶段,不通过发送通知
  1. 获取当前版本号备用
  2. 如果存在OLD_VSN的环境变量,跳到5, 否则下一步
  3. 通过ssh获取目标机器运行中的版本号
  4. 判断从ssh获取到的内容,如果是机器没有运行,则直接打包发布,否知下一步
  5. 对比当前版本和旧版本,相同则不需要升级和发布然后退出,不相同则下一步
  6. 通过代码生成relup发布包,然后ssh到目标机器升级版本

## 具体流程图
{% mermaid %}
graph TD
    start[开始] --> build_stage{编译阶段}
    notice[告知执行失败,异常结束]
    build_stage -->|成功| test_stage{测试阶段}
    build_stage -->|失败| notice
    test_stage -->|成功| deploy_stage(部署阶段)
    test_stage -->|失败| notice
    deploy_stage --> get_new_vsn[获取当前版本号备用]
    get_new_vsn --> is_env{存在OLD_VSN的环境变量}
    is_same_vsn{当前版本号和旧版本号相同}
    is_env -->|存在| is_same_vsn
    is_env -->|不存在| is_running{获取目标机器运行中的版本号}
    stop[结束]
    is_running -->|拿到版本号| is_same_vsn
    is_running -->|机器没有运行| release_tar_send_run[打包后发送到机器上直接启动]
    is_same_vsn -->|相同| stop
    is_same_vsn -->|不同| relup_tar[打包relup发布包发送到目标机器并升级版本]
    relup_tar --> stop
{% endmermaid %}

> note:上面这个流程图只是`DEV`,其他环境的会有不同,之后会说到

## 实现

### 创建项目

`rebar3 new app urf`, urf=upgrade release flow

### Makefile

```
APP_NAME = urf
all: compile test
APP_VER=$(shell awk '/release_vsn/{ print $$1 }' rebar.config | tr -d \")

###===================================================================
### build
###===================================================================
.PHONY: get-deps co compile run rel_tar

get-deps:
	rebar3 get-deps

co:compile
compile: get-deps
	rebar3 compile

### clean
.PHONY: clean distclean
clean:
	rebar3 clean

distclean: test_clean
	rebar3 clean -a

rel_tar:
	rebar3 tar

###===================================================================
### test
###===================================================================
.PHONY: test eunit ct test_shell test_run test_clean

test: epmd
	rebar3 do eunit -v, ct -v, cover -v

eunit: epmd
	rebar3 do eunit -v, cover

ct: epmd
	rebar3 do ct -v, cover

test_shell:
	rebar3 as test compile
	erl -pa _build/test/lib/*/ebin -pa _build/test/lib/$(APP_NAME)/test

test_run: epmd
	rebar3 as test shell

test_clean:
	@rm -rf _build/test/lib/$(APP_NAME)/test _build/test/logs _build/test/cover

###===================================================================
### relup
###===================================================================
.PHONY: tag up_vsn up_app_vsn up_relx_vsn clean_appup build_old_vsn relup check_appup

tag:
	git tag $(APP_VER)

up_vsn: up_app_vsn up_relx_vsn
up_app_vsn:
	@exec script/up_app_vsn.sh
up_relx_vsn:
	@exec script/up_relx_vsn.sh

clean_appup:
	rebar3 appup clean
	rm -f _build/default/lib/*/ebin/*.appup

build_old_vsn:
	@exec script/build_old_vsn.sh

relup:
	@exec script/relup.sh

check_appup:
	@exec script/check_appup.sh

###===================================================================
### other
###===================================================================
.PHONY: help tree logtail epmd

help:
	rebar3 help

tree:
	rebar3 tree

epmd:
	@pgrep epmd 2> /dev/null > /dev/null || epmd -daemon || true
```

上面部分除了比较常见的build & test 部分外还增加了relup的一些命令

* tag: 给当前版本打tag
* up_app_vsn: 修改`src/*.app.src`文件的版本号,最后一位+1
* up_relx_vsn: 修改`rebar.config`文件的版本号,最后一位+1
* up_vsn: up_app_vsn + up_relx_vsn
* clean_appup: 清理appup文件,打包relup文件前必须清理一次,不然appup插件检测到应用已经存在appup文件就不会再生成新的了
* build_old_vsn: 打包旧的版本,下面会放出脚本文件
* relup: 打包relup版本,下面会放出脚本文件
* check_appup: 打印所有应用下的appup文件

### rebar.config

```erlang
{erl_opts, [
    encrypt_debug_info
]}.

{plugins, [
    {rebar3_appup_plugin, "2.2.1"}
]}.
{deps, []}.

%% for test
{cover_enabled, true}.
{cover_print_enabled, true}.
{eunit_opts, [
    {dir, ["test/eunit"]}
]}.
{ct_opts, [
    {dir, ["test/ct"]},
    {readable, true}
]}.

%% for make release
{relx, [
    {include_erts, true},
    {system_libs, true},
    {dev_mode, false},
    {extended_start_script, true},

    {sys_config, "config/sys.config"},
    {vm_args, "config/vm.args"},

    {release, {urf,
        "0.1.0" %% release_vsn
    }, [urf]}
]}.

```

首先,应用发布升级,当然少不了增加appup插件:

```erlang
{plugins, [
    {rebar3_appup_plugin, "2.2.1"}
]}.
```

其次是增加编译保护:

```erlang
{erl_opts, [
    encrypt_debug_info
]}.
```

增加这个`encrypt_debug_info`的主要原因是因为我们编译的代码必须要带有debug_info,不然appup插件会报错,但是同时增加了debug_info之后编译出来的beam文件是可以被反编译的,所以需要增加保护,另外增加了这个只是保护了当前这个app,但是依赖的其他app并不会有保护,后面我们会讲到使用`rebar.config.script`文件来将deps的其他app也保护起来.

增加了上面这个保护还需要在项目根目录下增加一个`.erlang.crypt`文件,内容类似如下:

```
[{debug_info, des3_cbc, [], "12345678912345678912345678912345"}].
```

最后是增加relx:

```erlang
{relx, [
    {include_erts, true},
    {system_libs, true},
    {dev_mode, false},
    {extended_start_script, true},

    {sys_config, "config/sys.config"},
    {vm_args, "config/vm.args"},

    {release, {urf,
        "0.1.0" %% release_vsn
    }, [urf]}
]}.
```

可以看到版本好附近有一个注释`release_vsn`,这个注释的主要作用标记版本号,脚本会通过这个标记找到版本号

### rebar.config.script

```erlang
%%main(CONFIG) ->
    CONFIG_1 =
        case lists:keyfind(add_overrides, 1, CONFIG) of
            false -> CONFIG;
            {_, AddOverrides} ->
                Overrides = lists:concat([[{add, App, AddOverride} || App <- Apps] || {Apps, AddOverride} <- AddOverrides]),
                OldOverrides = proplists:get_value(overrides, CONFIG, []),
                lists:keystore(overrides, 1, CONFIG, {overrides, Overrides ++ OldOverrides})
        end,

    IsCI = os:getenv("CI_COMMIT_SHA") =/= false,
    IsRelup = os:getenv("RELUP_TAR") =/= false,

    ChangeProfileFun =
        fun(test, ProfileConfig0) -> {test, ProfileConfig0};
            (ProfileName, ProfileConfig0) ->
                ProfileConfig1 = maps:from_list(ProfileConfig0),
                Relx0 = maps:get(relx, ProfileConfig1, []),
                Relx2 =
                    case IsCI of
                        false -> Relx0;
                        _ ->
                            Relx1 = lists:keystore(include_erts, 1, Relx0, {include_erts, true}),
                            lists:keystore(system_libs, 1, Relx1, {system_libs, true})
                    end,
                Relx =
                    case IsRelup of
                        false -> Relx2;
                        _ ->
                            Relx3 = lists:keystore(include_erts, 1, Relx2, {include_erts, false}),
                            lists:keystore(system_libs, 1, Relx3, {system_libs, false})
                    end,
                ProfileConfig = maps:to_list(ProfileConfig1#{relx => Relx}),
                {ProfileName, ProfileConfig}
        end,

    Profiles0 = proplists:get_value(profiles, CONFIG_1, []),
    Profiles = [ChangeProfileFun(RebarProfile, ProfileConfig) ||
        {RebarProfile, ProfileConfig} <- Profiles0],
    CONFIG_2 = lists:keystore(profiles, 1, CONFIG_1, {profiles, Profiles}),
    {_, CONFIG_3} = ChangeProfileFun(none, CONFIG_2),

    LAST_CONFIG =
        case IsCI of
            false -> CONFIG_3;
            _ ->
                lists:keystore(global_rebar_dir, 1, CONFIG_3, {global_rebar_dir, "_build/rebar3"})
        end,
%%    io:format("~n~p~n", [LAST_CONFIG]),
    LAST_CONFIG.
```

`rebar.config.script`不熟悉的同学我这里简单介绍一下:

* 作用:

  通过执行erlang脚本动态的修改`rebar.config`的内容

* 运行原理

  在运行`rebar3`相关的命令之后,`rebar3`首先会读取`rebar.config`,然后将所有内容复制给`CONFIG`这个变量,然后再执行`rebar.config.script`这个文件,运行结束之后的结果就是最终的rebar的配置了

然后再来说说上面的代码:

* add_overrides主要是为了给deps的其他app加上保护,用法如下:

  增加到`rebar.config`文件里:

  ```erlang
  {add_overrides, [
      {[app1,app2,app3,...], [{erl_opts, [encrypt_debug_info]}]}
  ]}.
  ```

* 第二部分是根据环境变量来判断是否在ci环境和是否保护`RELUP_TAR`里

  - 一般不变更otp的版本和依赖不增加系统库的前提下,我们打包relup的包是不需要再包含`erts`和系统库,这样可以大大减少打包之后文件的大小, 因此我们在打包relup的时候,会在`export`变量 `RELUP_TAR` 到环境变量,这样只要在脚本里判断如果有这个变量则设置`include_erts`和`system_libs`为false.
  - 另外一般我们的开发机器是ubuntu,但是线上的环境是centos,所以我们本地编译线上的包的时候会制定centos版的erlang的位置,幸运的是,我们的ci的`runner`的环境也是centos,因此我们如果判断到是在ci环境里时,只要设置成true就行了.

* 在ci环境,我们会使用cache,但是gitlab的ci的cache只会保存项目下的文件,因此像一些保存在`~/.cache/rebar3`下的hex包就不会被保存,这样我们每次编译的时候都要去拉一遍hex的pkg,幸好,rebar3可以修改这些文件的位置,只要我们设置`global_rebar_dir`的位置为项目下的`_build/rebar3`,之后我们再设置cache保存这个文件夹就完美解决这个问题了.

> ps:可以看到第一行有一句注释,这句注释并不是必须的,其实它主要左右是我在idea里修改这个文件的时候,会打开这个注释,然后这个文件就变成了一个方法了,所有修改的时候格式也不会变化,同时也可以方便的用idea的格式化功能,算是一个小技巧,不过最后记得用完重新注释第一行,不然rebar3运行的时候会报错

### .gitlab.ci.yml

```yaml
image: cerl

before_script:
  - eval $(ssh-agent -s)
  - ssh-add <(echo "$SSH_PRIVATE_KEY")
  - mkdir -p ~/.ssh
  - echo -e "Host *\n\tStrictHostKeyChecking no\n\n" > ~/.ssh/config

after_script:
  - rm -rf _build/default/lib/urf
  - rm -rf ~/.git-credentials

stages:
  - build
  - test
  - deploy

cache:
  paths:
    - _build/default
    - _build/rebar3

build:
  stage: build
  except:
    - tags
  script:
    - make compile || ./script/ci_notice.sh "Build Fail"
  tags:
    - dockers

test:
  stage: test
  coverage: '/\|\s*total\s*\|\s*(\d+)%\s*\|/'
  except:
    - tags
  script:
    - make test || ./script/ci_notice.sh "Test Fail"
  tags:
    - dockers

deploy_dev:
  stage: deploy
  only:
    - /^d_dev$/
  except:
    - tags
  script:
    - ./script/ci_deploy.sh || ./script/ci_notice.sh "Deploy Fail"
  environment:
    name: dev
  variables:
    SSH_USER_HOST: "user@server"
    SERVER: "dev"
  tags:
    - dockers
```

对于`gitlab` 的 `CI` 不是很熟悉的同学可以看一下官方文档,文档里会给你很详尽的说明,同时也建议大家先系统性的学习一遍`.gitlab.ci.yml`文档的配置再回来看这篇文章.

之前说到过我们有三个阶段:

```yaml
stages:
  - build # 编译阶段
  - test # 测试阶段
  - deploy # 部署阶段
```

cache这一部分,ci有cache会大大减少整个流程的执行时间,这里cache了两个文件夹`default`和`rebar3`文件夹就够了

```yaml
cache:
  paths:
    - _build/default
    - _build/rebar3
```

build和test两个阶段比较简单,build就用`make compile`, test的话`make test`,另外如果想看到测试的代码覆盖率的话可以增加`coverage`,增加之后gitlab的页面那里就能显示这次的代码覆盖率了,其他就不一一详细说明了,主要来看看deploy阶段:

```yaml
deploy_dev:
  stage: deploy
  only:
    - /^d_dev$/
  except:
    - tags
  script:
    - ./script/ci_deploy.sh || ./script/ci_notice.sh "Deploy Fail"
  environment:
    name: dev
  variables:
    SSH_USER_HOST: "user@server"
    REBAR_PROFILE: "dev"
  tags:
    - dockers
```

这个阶段我们只针对`d_dev`这个分支,所以如果我们要部署或者升级`dev`环境,就只需要切换到这个分支然后合并最新的代码,最后推送这个分支到服务器,然后ci会帮你解决一切了,当然还是要通过我们写的脚本`ci_deploy.sh`来解决:

```shell
#!/usr/bin/env bash

case $OSTYPE in
    darwin*) SCRIPT=$(readlink $0 || true);;
    *) SCRIPT=$(readlink -f $0 || true);;
esac
[ -z $SCRIPT ] && SCRIPT=$0
SCRIPT_DIR="$(cd `dirname "$SCRIPT"` && pwd -P)"
cd "$SCRIPT_DIR/.."

if [ -z $SSH_USER_HOST ]
then
    echo "empty SSH_USER_HOST"
    exit 1
fi
if [ -z $SERVER ]
then
    echo "empty SERVER"
    exit 1
fi

REL_NAME=$(awk -F '[,{]' '/\{release, \{/{ print $4 }' rebar.config)
NEW_VSN=$(awk '/release_vsn/{ print $1 }' rebar.config | tr -d \")

[ -z $TARGET_DIR ] && TARGET_DIR="/data/$REL_NAME"

export TARGET_DIR=$TARGET_DIR
export REBAR_PROFILE=$SERVER
TAR_FILE0="$REL_NAME-$NEW_VSN.tar.gz"
TAR_FILE="_build/$REBAR_PROFILE/rel/$REL_NAME/$TAR_FILE0"

export OLD_VSN=`exec $SCRIPT_DIR/ssh_get_vsn.sh`

only_send_and_start() {
    echo "rebar3 tar" && \
    rebar3 tar && \
    echo "scp $TAR_FILE $SSH_USER_HOST:$TARGET_DIR" && \
    scp $TAR_FILE $SSH_USER_HOST:$TARGET_DIR && \
    echo "ssh $SSH_USER_HOST \"cd $TARGET_DIR && tar zxf $TAR_FILE0 && rm -f $TAR_FILE0 && ./bin/$REL_NAME start\"" && \
    ssh $SSH_USER_HOST "cd $TARGET_DIR && tar zxf $TAR_FILE0 && rm -f $TAR_FILE0 && ./bin/$REL_NAME start" && \
    echo "run successfully"
}

case $OLD_VSN in
    "miss_file")
        only_send_and_start;;
    "Node is not running!")
        only_send_and_start;;
    $NEW_VSN)
        echo "same vsn : $NEW_VSN";;
    *)
        $SCRIPT_DIR/build_old_vsn.sh && \
        echo "rebar3 appup clean" && \
        rebar3 appup clean && \
        echo "rm -f _build/default/lib/*/ebin/*.appup" && \
        rm -f _build/default/lib/*/ebin/*.appup && \
        echo "rebar3 get-deps" && \
        rebar3 get-deps && \
        echo "rebar3 release" && \
        rebar3 release && \
        echo "rebar3 appup compile" && \
        rebar3 appup compile && \
        echo "rebar3 appup generate --previous_version $OLD_VSN" && \
        rebar3 appup generate --previous_version $OLD_VSN && \
        echo "rebar3 relup --upfrom $OLD_VSN" && \
        rebar3 relup --upfrom $OLD_VSN && \
        echo "rebar3 tar" && \
        rebar3 tar && \
        echo "scp $TAR_FILE $SSH_USER_HOST:$TARGET_DIR/releases" && \
        scp $TAR_FILE $SSH_USER_HOST:$TARGET_DIR/releases && \
        echo "ssh $SSH_USER_HOST \"cd $TARGET_DIR && ./bin/$REL_NAME upgrade $NEW_VSN\"" && \
        ssh $SSH_USER_HOST "cd $TARGET_DIR && ./bin/$REL_NAME upgrade $NEW_VSN" && \
        echo "upgrade successfully";;
esac
```

`ssh_get_vsn.sh`:

```shell
#!/usr/bin/env bash

case $OSTYPE in
    darwin*) SCRIPT=$(readlink $0 || true);;
    *) SCRIPT=$(readlink -f $0 || true);;
esac
[ -z $SCRIPT ] && SCRIPT=$0
SCRIPT_DIR="$(cd `dirname "$SCRIPT"` && pwd -P)"
cd "$SCRIPT_DIR/.."

if [ -z $SSH_USER_HOST ]
then
    echo "empty SSH_USER_HOST"
    exit 1
fi
if [ -z $TARGET_DIR ]
then
    echo "empty TARGET_DIR"
    exit 1
fi

REL_NAME=$(awk -F '[,{]' '/\{release, \{/{ print $4 }' rebar.config)

ssh_cmd_get_versions="([ -f $TARGET_DIR/bin/$REL_NAME ] && cd $TARGET_DIR ; ./bin/$REL_NAME versions | tr -d '\n') || ([ ! -f $TARGET_DIR/bin/$REL_NAME ] && mkdir -p $TARGET_DIR ; echo miss_file)"
#echo "ssh $SSH_USER_HOST \"${ssh_cmd_get_versions}\""
versions=`ssh $SSH_USER_HOST "${ssh_cmd_get_versions}"`

case $versions in
    "miss_file")
        echo "miss_file";;
    "Node is not running!")
        echo "Node is not running!";;
    *)
        echo $(echo $versions | sed -r "s/.*\*\ ([0-9]+.[0-9]+.[0-9]+)\ permanent.*/\1/g")
esac
```

上面的这些脚本其实只是之前的流程的转义,我这里就不重复了.

-------

今天就这样~玩得开心!

end
