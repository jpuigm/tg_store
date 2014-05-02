* Testing out slave:start/2 times out,

```
(bar@jobs)1> slave:start_link(jobs, pepito).
{error,timeout}
```