* Sending a request to add a new pair tag value to a remote node that is down, will throw an exception,

```
(tg_store@127.0.0.1)18> tg_store_server:add(a, "jorge", [node(), 'a1@localhost']).
16:13:01.355 [info] Added {a,"jorge"} to the store
** exception exit: {{nodedown,a1@localhost},
                    {gen_server,call,
                                [{node,a1@localhost},{add,a,"jorge"},1000]}}
     in function  gen_server:call/3 (gen_server.erl, line 188)
     in call from tg_store_server:add/4 (src/tg_store_server.erl, line 102)
(tg_store@127.0.0.1)19> 16:13:01.355 [error] ** System running to use fully qualified hostnames **
** Hostname localhost is illegal **
```