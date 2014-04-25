# tg_store #

## Distributed Associative Store ##

Associative store in which values are associated with tags. It is possible to store a tag/value pair, and to look up the value(s) associated with a tag.
Its distributed nature, makes it able to replicate tag/value pair across different nodes.

## Usage ##

Having the following two nodes,

```
$ tg_store/bin/tg_store console -sname bar
Erlang R16B03-1 (erts-5.10.4) [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

23:11:20.180 [info] Application lager started on node bar@jobs
23:11:20.180 [info] Application tg_store started on node bar@jobs
```

```
$ tg_store/bin/tg_store console -sname foo
Erlang R16B03-1 (erts-5.10.4) [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

23:11:45.535 [info] Application lager started on node foo@jobs
23:11:45.535 [info] Application tg_store started on node foo@jobs
```

we can add a tag/value pair to `bar`, that will automatically be replicated to `foo`

```
(bar@jobs)1> tg_store_server:add(a, "jorge", [node(), 'foo@jobs']).
23:12:03.648 [info] Added {a,"jorge"} to the store
23:12:03.661 [info] Tag a successfully stored and replicated
ok
```

```
(foo@jobs)1> 23:12:03.661 [info] Added {a,"jorge"} to the store

(foo@jobs)1>
(foo@jobs)1> tg_store_server:lookup(a).
{ok,"jorge"}
23:12:20.928 [info] Lookup of key: a with result: {ok,"jorge"}
```






