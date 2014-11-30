-module(rev_reloader).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2, format_status/2]).

-record(state, {
	monitor
}).

start_link() -> gen_server:start_link(?MODULE, [], []).

% gen_server

init([]) ->
	RunningApplications = [Application || {Application, _, _} <- application:which_applications()],
	AppResourceFiles = [code:where_is_file(lists:flatten(io_lib:format("~s.app", [App]))) || App <- RunningApplications],
	AppProperties = [file:consult(ResourceFile) || ResourceFile <- AppResourceFiles],
	ActiveModules = lists:usort(lists:flatten([proplists:get_value(modules, Properties) || {ok, [{application, _, Properties}]} <- AppProperties])),
	_ = [code:ensure_loaded(Mod) || Mod <- ActiveModules],
	ActiveModules = lists:usort(lists:flatten([proplists:get_value(modules, Properties) || {ok, [{application, _, Properties}]} <- AppProperties])),
	ModuleMap = [{Mod, code:which(Mod)} || Mod <- ActiveModules],
	CodeDirs = lists:usort([filename:dirname(Path) || {_Mod, Path} <- ModuleMap, Path =/= preloaded]),
	SrcDirs = lists:usort([filename:dirname(proplists:get_value(source, Mod:module_info(compile))) ||
	                       {Mod, Path} <- ModuleMap, Path =/= preloaded, erlang:function_exported(Mod, module_info, 0)]),
	WatchableSrcDirs = [Dir || Dir <- SrcDirs, filelib:is_dir(Dir)],
	WatchList = CodeDirs ++ WatchableSrcDirs,
	error_logger:info_report([{watched_dirs, WatchList}]),
	Cmd = "inotifywait -r -m -q -e close_write,moved_to --format \"%w%f\" " ++ string:join(WatchList, " "),
	{ok, Pid} = gen_os_proc:start_link(Cmd, [{proc_opts, [{hibernate_timeout, 1000}]}]),
	{ok, #state{monitor = Pid}}.

terminate(_Reason, _State) -> ok.

handle_call(_Req, _From, State) -> {stop, unexpected, State}.

handle_cast(_Req, State) -> {stop, unexpected, State}.

handle_info({line, Monitor, Path}, #state{monitor = Monitor} = State) ->
	Extension = filename:extension(Path),
	case Extension of
		".erl" ->
			Module = list_to_atom(filename:basename(Path, ".erl")),
			recompile(Module, Path, fun compile_erl/3);
		".dtl" ->
			Module = list_to_atom(filename:basename(Path, ".dtl") ++ "_dtl"),
			recompile(Module, Path, fun compile_dtl/3);
		".beam" -> reload(Path);
		_ -> ignore
	end,
	{noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

format_status(_Opt, [_PDict, State]) ->
	FormattedState = lists:zip(record_info(fields, state), tl(tuple_to_list(State))),
	[{data, [{"State", FormattedState}]}].

% private

recompile(Module, Path, CompileFun) ->
	case erlang:function_exported(Module, module_info, 1) of
		true ->
			CompileOpts = proplists:get_value(options, Module:module_info(compile), []),
			do_recompile(Module, Path, CompileFun, CompileOpts);
		false ->
			error_logger:warning_report([{ignored, Path}])
	end.

do_recompile(Module, Path, CompileFun, CompileOpts) ->
	case CompileFun(Module, Path, [binary, return | CompileOpts]) of
		{ok, _ModName, ModBin, Warnings} ->
			file:write_file(code:which(Module), ModBin),
			error_logger:info_report([{recompile, Path}, {warnings, Warnings}]);
		{error, Errors, Warnings} ->
			error_logger:error_report([{recompile, Path}, {errors, Errors}, {warnings, Warnings}])
	end.

compile_erl(_Module, Path, CompileOpts) -> compile:file(Path, CompileOpts).

compile_dtl(Module, Path, CompileOpts) ->
	case code:which(erlydtl) of
		non_existing ->
			{error, [no_erlydtl_compiler], []};
		_ ->
			erlydtl:compile_file(Path, Module, [{out_dir, false}, no_load, force_recompile | CompileOpts])
	end.

reload(Path) ->
	Module = erlang:list_to_atom(filename:basename(Path, ".beam")),
	code:purge(Module),
	code:load_file(Module),
	error_logger:info_report([{reloaded, Module}]),
	case erlang:function_exported(Module, '$on_reload', 0) of
		true ->
			try apply(Module, '$on_reload', []) of
				Result ->
					error_logger:info_report([
						{reload_trigger, {Module, '$on_reload', []}},
						{result, Result}
					])
			catch
				Type:Error ->
					error_logger:error_report([
						{reload_trigger, {Module, '$on_reload', []}},
						{error, {Type, Error}},
						{stacktrace, erlang:get_stacktrace()}
					])
			end;
		false -> ok
	end.
