# erlang-function-deps
Get all functions that are being called from a function

#Usage

```
$ make app shell

Erlang/OTP 17 [erts-6.1] [source-d2a4c20] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Starting Sync (Automatic Code Compiler / Reloader)
Scanning source files...
Eshell V6.1  (abort with ^G)
1> function_deps:get(ktn_code, parse_tree, 1).
[parse_tree]
2> function_deps:get(ktn_code, parse_tree, 2).
[to_str,
 {erl_scan,string},
 {aleppo,process_tokens},
 {lists,partition},
 split_when,
 {lists,map},
 to_map]
3>
```
