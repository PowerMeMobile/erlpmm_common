-module(pmm_conf).

-behaviour(gen_server).

-compile({no_auto_import, [get/1]}).

-export([start_link/1,
	 start_link/2,
	 stop/0,
	 get/1,
	 get/2,
	 temporary_set/2,
	 reload/1,
	 log_app_env/1
]).

-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3]).

-ifdef(TEST).
-export([get_changed_settings/2]).
-endif.

-record(st, {
      app :: atom(),
      keysToPreserve :: list(),
	  local_settings :: list(),
	  cron_jobs :: list(reference)
}).

-define(gv(Key, List), proplists:get_value(Key, List)).
-define(set(Key, List), proplists:is_defined(Key, List)).

%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------

-spec start_link({atom(), list()}) -> {'ok', pid()} | {'error', any()}.
start_link({App, KeysToPreserve}) ->
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [{App, KeysToPreserve}], []) of
	{ok, Pid} -> {ok, Pid};
	{error, {already_started, Pid}} -> 
	    link(Pid),
	    {ok, Pid}
    end.

-spec start_link({atom(), list()}, list()) -> {'ok', pid()} | {'error', any()}.
start_link({App, KeysToPreserve}, LocalSettings) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [{App, KeysToPreserve}, LocalSettings], []).

-spec stop() -> 'ok'.
stop() ->
    gen_server:cast(?MODULE, stop).

-spec get(atom()) -> term().
get(Key) ->
    gen_server:call(?MODULE, {get, Key}, infinity).

-spec get(atom(), term()) -> term().
get(Key, Default) ->
    gen_server:call(?MODULE, {get, Key, Default}, infinity).

-spec temporary_set(atom(), term()) -> ok.
temporary_set(Key, Value) ->
    gen_server:cast(?MODULE, {temporary_set, Key, Value}).

-spec reload(atom()) -> ok.
reload(App) ->
    gen_server:cast(?MODULE, {reload, App}).

log_app_env(App) ->
    [lager:info("env ~p -> ~p -> ~p\n", [App, K, V])
        || {K, V} <- lists:sort(application:get_all_env(App))].

%% -------------------------------------------------------------------------
%% gen_server callbacks
%% -------------------------------------------------------------------------

init([{App, KeysToPreserve}]) ->
    lager:info("config: initializing ~s", [App]),
    LocalSettings = load_local_config_settings(App),
    init([{App, KeysToPreserve}, LocalSettings]);

init([{App, KeysToPreserve}, LocalSettings]) ->
    CronJobs = apply_configured_cron_tab(App, LocalSettings),
    {ok, #st{
             app = App,
             keysToPreserve = KeysToPreserve, 
             local_settings = LocalSettings, 
             cron_jobs = CronJobs
            }
    }.

terminate(Reason, St) ->
    cancel_cron_jobs(St#st.cron_jobs),
    lager:info("config: terminated (~w)", [Reason]).

handle_call({get, Key}, _From, St) ->
    LocalResult = check_in_local_settings(Key, St#st.local_settings),
    case LocalResult of
	undefined ->
	    Result = case application:get_env(St#st.app, Key, LocalResult) of
			 undefined -> exit({application_parameter_undefined, Key});
 			 Value -> Value
		     end,
	    {reply, Result, St};
	_ ->
	    {reply, LocalResult, St}
    end;

handle_call({get, Key, Default}, _From,  St) ->
    {reply, internal_get(St#st.app, Key, Default, St#st.local_settings), St}.

handle_cast(stop, St) ->
    {stop, normal, St};

handle_cast({temporary_set, Key, Value}, St) ->
    application:set_env(St#st.app, Key, Value),
    {noreply, St};

handle_cast({reload, App}, St) ->
    
    AppSt = St#st.app,

    %We store in application environment some info what absent in app.config.

    KeysToPreserve = 
	case App of 
	    AppSt -> St#st.keysToPreserve; 
	    _ -> [] 
	end,
    DataToPreserve = [{K, application:get_env(App, K)} || K <- KeysToPreserve],
    case App of
	erlcron -> {ok, [App]} = corman:reload([App], [App]);
	_ -> {ok, [App]} = corman:reload([App], [])
    end,
    lists:foreach(fun
		      ({Key, {ok, Value}}) -> application:set_env(App, Key, Value);
		      ({_Key, undefined}) -> ok 
		  end, 
		  DataToPreserve),

    NewLocalSettings =
	case App of
	    AppSt -> 
		Loaded = load_local_config_settings(St#st.app),
		log_local_settings_change(St#st.local_settings, Loaded),
		Loaded;
	    _ -> St#st.local_settings
	end,
    NewCronJobs = 
	case App of
	    erlcron -> reapply_configured_cron_tab(St);
	    AppSt -> reapply_configured_cron_tab(St);
	    _ -> St#st.cron_jobs
	end,	
    {noreply, St#st{local_settings = NewLocalSettings, cron_jobs = NewCronJobs}}.

handle_info(Info, St) ->
    {stop, {unexpected_info, Info}, St}.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%% -------------------------------------------------------------------------
%% Private functions
%% -------------------------------------------------------------------------
internal_get(App, Key, Default, LocalSettings) ->
    case check_in_local_settings(Key, LocalSettings) of
	undefined -> application:get_env(App, Key, Default);
	LocalResult -> LocalResult
    end.

load_local_config_settings(App) ->
    FileName = application:get_env(App, local_config_path, "etc/app_local.config"),
    case file:consult(FileName) of
	{ok, PropListInFile} -> 
	    lager:info("config: local config from ~s loaded successfully", [FileName]),
	    PropListInFile;
	{error, enoent} ->
	    lager:info("config: local config at ~s is abscent", [FileName]),
	    [];
	{error, Reason} ->
	    lager:error("config: can't read local config file ~s (~w)", [FileName, Reason]),
	    []
    end.

check_in_local_settings(Key, LocalSettings) ->
    proplists:get_value(Key, LocalSettings).

reapply_configured_cron_tab(St) ->
    cancel_cron_jobs(St#st.cron_jobs),
    apply_configured_cron_tab(St#st.app, St#st.local_settings).

apply_configured_cron_tab(App, LocalSettings) ->
    CronTab = internal_get(App, crontab, [], LocalSettings),
    lists:map(fun (JobSpec) -> erlcron:cron(JobSpec) end, CronTab).

cancel_cron_jobs(JobRefs) ->
    lists:foreach(fun (JobRef) -> erlcron:cancel(JobRef) end, JobRefs).

log_local_settings_change(Before, After) ->
    LogChanged =
	fun (Key) -> lager:info("~w changed from ~w to ~w in local_config",
				[Key, ?gv(Key, Before), ?gv(Key, After)])
	end,
    LogRemoved =
	fun (Key) -> lager:info("~w (~w) was removed from local config",
				[Key, ?gv(Key, Before)])
	end,
    LogAdded = 
	fun (Key) -> lager:info("~w (~w) was added to local config",
				[Key, ?gv(Key, After)])
	end,
    {Changed, Removed, Added} = get_changed_settings(Before, After),
    lists:foreach(LogChanged, Changed),
    lists:foreach(LogRemoved, Removed),
    lists:foreach(LogAdded, Added).

get_changed_settings(Before, After) ->
    KeysSet = sets:from_list(proplists:get_keys(Before) ++ proplists:get_keys(After)),
    Keys = sets:to_list(KeysSet),
    KeyIsAdded = fun (Key) -> ?set(Key, After) andalso not ?set(Key, Before) end,
    KeyIsRemoved = fun (Key) -> ?set(Key, Before) andalso not ?set(Key, After) end,
    KeyIsChanged = fun (Key) -> 
			   ?set(Key, Before) andalso ?set(Key, After) 
			       andalso ?gv(Key, Before) =/= ?gv(Key, After) 
		   end,
    Changed = lists:filter(KeyIsChanged, Keys),
    Removed= lists:filter(KeyIsRemoved, Keys),
    Added = lists:filter(KeyIsAdded, Keys),
    {Changed, Removed, Added}.
				   
    
