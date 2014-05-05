* Testing out slave:start/2 times out,

```
(bar@jobs)1> slave:start_link(jobs, pepito).
{error,timeout}
```

Researching further about this issue, there seems to be an issue with the erlang bundled when doing a release,

```
$ rel/tg_store/bin/tg_store console -sname bar
Exec: /Users/juan/dev/src/tg_store/rel/tg_store/erts-5.10.4/bin/erlexec -boot /Users/juan/dev/src/tg_store/rel/tg_store/releases/0.0.1/tg_store -mode embedded -config /Users/juan/dev/src/tg_store/rel/tg_store/releases/0.0.1/sys.config -args_file /Users/juan/dev/src/tg_store/rel/tg_store/releases/0.0.1/vm.args -- console -sname bar
Root: /Users/juan/dev/src/tg_store/rel/tg_store
Erlang R16B03-1 (erts-5.10.4) [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]
22:03:21.123 [info] Application lager started on node bar@jobs
22:03:21.123 [info] Application tg_store started on node bar@jobs
(bar@jobs)1> application:which_applications().
[{tg_store,"Distributed Associative Store","0.0.1"},
 {lager,"Erlang logging framework","2.0.3"},
 {goldrush,"Erlang event stream processor","0.1.6"},
 {compiler,"ERTS  CXC 138 10","4.9.4"},
 {syntax_tools,"Syntax tools","1.6.13"},
 {sasl,"SASL  CXC 138 11","2.3.4"},
 {stdlib,"ERTS  CXC 138 10","1.19.4"},
 {kernel,"ERTS  CXC 138 10","2.16.4"}]
(bar@jobs)3> slave:start(jobs, pepito).
(<0.1085.0>) call slave:start(jobs,pepito)
(<0.1085.0>) call slave:start(jobs,pepito,[])
(<0.1085.0>) call slave:start(jobs,pepito,[],no_link)
(<0.1085.0>) call slave:start(jobs,pepito,[],no_link,tg_store)
(<0.1085.0>) call slave:to_list(jobs)
(<0.1085.0>) returned from slave:to_list/1 -> "jobs"
(<0.1085.0>) call slave:strip_host_name("jobs")
(<0.1085.0>) call slave:strip_host_name("obs")
(<0.1085.0>) call slave:strip_host_name("bs")
(<0.1085.0>) call slave:strip_host_name("s")
(<0.1085.0>) call slave:strip_host_name([])
(<0.1085.0>) returned from slave:strip_host_name/1 -> []
(<0.1085.0>) returned from slave:strip_host_name/1 -> "s"
(<0.1085.0>) returned from slave:strip_host_name/1 -> "bs"
(<0.1085.0>) returned from slave:strip_host_name/1 -> "obs"
(<0.1085.0>) returned from slave:strip_host_name/1 -> "jobs"
(<0.1085.0>) call slave:start_it("jobs",pepito,pepito@jobs,[],no_link,tg_store)
(<0.1095.0>) call slave:wait_for_slave(<0.1085.0>,"jobs",pepito,pepito@jobs,[],no_link,tg_store)
(<0.1095.0>) call slave:register_unique_name(0)
(<0.1095.0>) returned from slave:register_unique_name/1 -> slave_waiter_0
(<0.1095.0>) call slave:mk_cmd("jobs",pepito,[],slave_waiter_0,tg_store)
(<0.1095.0>) call slave:long_or_short()
(<0.1095.0>) returned from slave:long_or_short/0 -> " -sname "
(<0.1095.0>) call slave:after_char(64,"bar@jobs")
(<0.1095.0>) call slave:after_char(64,"ar@jobs")
(<0.1095.0>) call slave:after_char(64,"r@jobs")
(<0.1095.0>) call slave:after_char(64,"@jobs")
(<0.1095.0>) returned from slave:after_char/2 -> "jobs"
(<0.1095.0>) returned from slave:after_char/2 -> "jobs"
(<0.1095.0>) returned from slave:after_char/2 -> "jobs"
(<0.1095.0>) returned from slave:after_char/2 -> "jobs"
(<0.1095.0>) returned from slave:mk_cmd/5 -> {ok,
                                              "tg_store -detached -noinput -master bar@jobs  -sname pepito@jobs -s slave slave_start bar@jobs slave_waiter_0 "}
(<0.1095.0>) returned from slave:wait_for_slave/7 -> {result,{error,timeout}}
(<0.1085.0>) returned from slave:start_it/6 -> {error,timeout}
(<0.1085.0>) returned from slave:start/5 -> {error,timeout}
(<0.1085.0>) returned from slave:start/4 -> {error,timeout}
(<0.1085.0>) returned from slave:start/3 -> {error,timeout}
(<0.1085.0>) returned from slave:start/2 -> {error,timeout}
{error,timeout}
```

where as using a clean erlang node,

```
$ erl -sname foo
Erlang R16B03-1 (erts-5.10.4) [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V5.10.4  (abort with ^G)
(foo@jobs)1> application:which_applications().
[{stdlib,"ERTS  CXC 138 10","1.19.4"},
 {kernel,"ERTS  CXC 138 10","2.16.4"}]
(foo@jobs)3> slave:start(jobs, pepito).
(<0.38.0>) call slave:start(jobs,pepito)
(<0.38.0>) call slave:start(jobs,pepito,[])
(<0.38.0>) call slave:start(jobs,pepito,[],no_link)
(<0.38.0>) call slave:start(jobs,pepito,[],no_link,erl)
(<0.38.0>) call slave:to_list(jobs)
(<0.38.0>) returned from slave:to_list/1 -> "jobs"
(<0.38.0>) call slave:strip_host_name("jobs")
(<0.38.0>) call slave:strip_host_name("obs")
(<0.38.0>) call slave:strip_host_name("bs")
(<0.38.0>) call slave:strip_host_name("s")
(<0.38.0>) call slave:strip_host_name([])
(<0.38.0>) returned from slave:strip_host_name/1 -> []
(<0.38.0>) returned from slave:strip_host_name/1 -> "s"
(<0.38.0>) returned from slave:strip_host_name/1 -> "bs"
(<0.38.0>) returned from slave:strip_host_name/1 -> "obs"
(<0.38.0>) returned from slave:strip_host_name/1 -> "jobs"
(<0.38.0>) call slave:start_it("jobs",pepito,pepito@jobs,[],no_link,erl)
(<0.48.0>) call slave:wait_for_slave(<0.38.0>,"jobs",pepito,pepito@jobs,[],no_link,erl)
(<0.48.0>) call slave:register_unique_name(0)
(<0.48.0>) returned from slave:register_unique_name/1 -> slave_waiter_0
(<0.48.0>) call slave:mk_cmd("jobs",pepito,[],slave_waiter_0,erl)
(<0.48.0>) call slave:long_or_short()
(<0.48.0>) returned from slave:long_or_short/0 -> " -sname "
(<0.48.0>) call slave:after_char(64,"foo@jobs")
(<0.48.0>) call slave:after_char(64,"oo@jobs")
(<0.48.0>) call slave:after_char(64,"o@jobs")
(<0.48.0>) call slave:after_char(64,"@jobs")
(<0.48.0>) returned from slave:after_char/2 -> "jobs"
(<0.48.0>) returned from slave:after_char/2 -> "jobs"
(<0.48.0>) returned from slave:after_char/2 -> "jobs"
(<0.48.0>) returned from slave:after_char/2 -> "jobs"
(<0.48.0>) returned from slave:mk_cmd/5 -> {ok,
                                            "erl -detached -noinput -master foo@jobs  -sname pepito@jobs -s slave slave_start foo@jobs slave_waiter_0 "}
(<0.48.0>) call slave:slave_started(<0.38.0>,no_link,<7198.38.0>)
(<0.48.0>) returned from slave:slave_started/3 -> {result,{ok,pepito@jobs}}
(<0.48.0>) returned from slave:wait_for_slave/7 -> {result,{ok,pepito@jobs}}
(<0.38.0>) returned from slave:start_it/6 -> {ok,pepito@jobs}
(<0.38.0>) returned from slave:start/5 -> {ok,pepito@jobs}
(<0.38.0>) returned from slave:start/4 -> {ok,pepito@jobs}
(<0.38.0>) returned from slave:start/3 -> {ok,pepito@jobs}
{ok,pepito@jobs}
(<0.38.0>) returned from slave:start/2 -> {ok,pepito@jobs}
```
