{module, add}.  %% version = 0

{exports, [{add,2},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 9}.


{function, add, 2, 2}.
  {label,1}.
    {line,[{location,"add.erl",4}]}.
    {func_info,{atom,add},{atom,add},2}.
  {label,2}.
    {allocate,1,2}.
    {move,{x,1},{y,0}}.
    {call,1,{f,4}}. % id/1
    {swap,{y,0},{x,0}}.
    {call,1,{f,4}}. % id/1
    {gc_bif,'+',{f,0},1,[{y,0},{x,0}],{x,0}}.
    {deallocate,1}.
    return.


{function, id, 1, 4}.
  {label,3}.
    {line,[{location,"add.erl",6}]}.
    {func_info,{atom,add},{atom,id},1}.
  {label,4}.
    return.


{function, module_info, 0, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,add},{atom,module_info},0}.
  {label,6}.
    {move,{atom,add},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 8}.
  {label,7}.
    {line,[]}.
    {func_info,{atom,add},{atom,module_info},1}.
  {label,8}.
    {move,{x,0},{x,1}}.
    {move,{atom,add},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
