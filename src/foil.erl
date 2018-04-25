-module(foil).
-include("foil.hrl").

-compile(inline).
-compile({inline_size, 512}).

-ignore_xref([
    {foil_modules, lookup, 1}
]).


-export([
    delete/1,
    delete/2,
    insert/3,
    load/1,
    lookup/2,
    new/1
]).

%% public
-spec delete(namespace()) ->
    ok | error().

delete(Namespace) ->
    try foil_modules:lookup(Namespace) of
        {ok, Module} ->
            ets:delete(Module),
            ets:delete(?FOIL_TABLE, Namespace),
            KVs = ets:tab2list(?FOIL_TABLE),
            foil_compiler:load(foil_modules, KVs),
            ok;
        {error, key_not_found} ->
            {error, module_not_found}
    catch
        error:undef ->
            {error, foil_not_started}
    end.

-spec delete(namespace(), key()) ->
    ok | error().

delete(Namespace, Key) ->
    try foil_modules:lookup(Namespace) of
        {ok, Module} ->
            ets:delete(Module, Key),
            ok;
        {error, key_not_found} ->
            {error, module_not_found}
    catch
        error:undef ->
            {error, foil_not_started}
    end.

-spec insert(namespace(), key(), value()) ->
    ok | error().

insert(Namespace, Key, Value) ->
    try foil_modules:lookup(Namespace) of
        {ok, Module} ->
            ets:insert(Module, {Key, Value}),
            ok;
        {error, key_not_found} ->
            {error, module_not_found}
    catch
        error:undef ->
            {error, foil_not_started}
    end.

-spec load(namespace()) ->
    ok | error().

load(Namespace) ->
    try foil_modules:lookup(Namespace) of
        {ok, Module} ->
            KVs = ets:tab2list(Module),
            foil_compiler:load(Module, KVs);
        {error, key_not_found} ->
            {error, module_not_found}
    catch
        error:undef ->
            {error, foil_not_started}
    end.

-spec lookup(namespace(), key()) ->
    {ok, value()} | error().

lookup(Namespace, Key) ->
    try foil_modules:lookup(Namespace) of
        {ok, Module} ->
            case Module:lookup(Key) of
                {ok, Result} -> case is_list(Result) of
                                    true -> case lists:sublist(Result, 5) of 
                                                "#Ref<" -> {ok, list_to_ref(Result)};
                                                _ -> {ok, convert(Result)}
                                            end;
                                    false -> {ok, Result}
                                end;
                {error, key_not_found} ->{error, key_not_found}
            end;
        {error, key_not_found} ->
            {error, module_not_found}
    catch
        error:undef ->
            {error, foil_not_started}
    end.

-spec new(namespace()) ->
    ok | error().

new(Namespace) ->
    try foil_modules:lookup(Namespace) of
        {ok, _Module} ->
            {error, module_exists};
        {error, key_not_found} ->
            Module = module(Namespace),
            ets:new(Module, [named_table, public]),
            Server = whereis(foil_server),
            ets:give_away(Module, Server, undefined),
            ets:insert(?FOIL_TABLE, {Namespace, Module}),
            KVs = ets:tab2list(?FOIL_TABLE),
            foil_compiler:load(foil_modules, KVs)
    catch
        error:undef ->
            {error, foil_not_started}
    end.
%% private
convert([])->[];
convert([List])->
    case is_list(List) of 
        false -> [List];
        true -> case lists:sublist(List, 5) of 
                     "#Ref<" ->  [list_to_ref(List)];
                      _ -> [List]
                end
    end;           
convert(List)->
    case is_list(List) of 
        true -> case lists:sublist(List, 5) of 
                     "#Ref<" ->  list_to_ref(List);
                      _ -> [Head|Tail] = List,
                            case is_list(Head) of 
                               true -> case lists:sublist(Head, 5) of 
                                             "#Ref<" ->  [list_to_ref(Head)|convert(Tail)];
                                             _ -> [convert(Head)|convert(Tail)]
                                       end;
                               false -> [Head|convert(Tail)]
                            end;
                 end;
        false -> List
    end.
module(Namespace) ->
    list_to_atom(atom_to_list(Namespace) ++ "_foil").
