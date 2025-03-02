{module, useless}.  %% version = 0

{exports, [{add,2},
           {greet_and_add_two,1},
           {hello,0},
           {module_info,0},
           {module_info,1}]}.

{attributes, [{myown,[aboba]}]}.

{labels, 11}.


{function, add, 2, 2}.
  {label,1}.
    {line,[{location,"useless.erl",7}]}.
    {func_info,{atom,useless},{atom,add},2}.
  {label,2}.
    {line,[{location,"useless.erl",8}]}.
    {gc_bif,'+',{f,0},2,[{x,0},{x,1}],{x,0}}.
    return.


{function, hello, 0, 4}.
  {label,3}.
    {line,[{location,"useless.erl",12}]}.
    {func_info,{atom,useless},{atom,hello},0}.
  {label,4}.
    {move,{literal,"Hello, world!~n"},{x,0}}.
    {line,[{location,"useless.erl",13}]}.
    {call_ext_only,1,{extfunc,io,format,1}}.


{function, greet_and_add_two, 1, 6}.
  {label,5}.
    {line,[{location,"useless.erl",15}]}.
    {func_info,{atom,useless},{atom,greet_and_add_two},1}.
  {label,6}.
    {allocate,1,1}.
    {move,{x,0},{y,0}}.
    {line,[{location,"useless.erl",16}]}.
    {call,0,{f,4}}. % hello/0
    {move,{integer,2},{x,1}}.
    {move,{y,0},{x,0}}.
    {call_last,2,{f,2},1}. % add/2


{function, module_info, 0, 8}.
  {label,7}.
    {line,[]}.
    {func_info,{atom,useless},{atom,module_info},0}.
  {label,8}.
    {move,{atom,useless},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 10}.
  {label,9}.
    {line,[]}.
    {func_info,{atom,useless},{atom,module_info},1}.
  {label,10}.
    {move,{x,0},{x,1}}.
    {move,{atom,useless},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
