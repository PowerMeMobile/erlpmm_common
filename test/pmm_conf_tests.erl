-module(pmm_conf_tests).

-include_lib("eunit/include/eunit.hrl").

test1_local_config_empty_test() ->
    PmmConfPid = set_up_config([]),
    ?assertEqual(false, pmm_conf:get(strip_leading_zero)),
    ?assertEqual("999", pmm_conf:get(country_code)),
    ?assertEqual(undefined, pmm_conf:get(foo_bar, undefined)),
    ?assertEqual(default, pmm_conf:get(foo_bar, default)),
    pmm_conf:stop(),
    wait_for_exit(PmmConfPid)
.

test2_local_config_is_not_empty_test() ->
    PmmConfPid = set_up_config([{key_from_local_config, value_from_local_config}]),
    ?assertEqual(value_from_local_config, pmm_conf:get(key_from_local_config)),
    ?assertEqual(false, pmm_conf:get(strip_leading_zero)),
    ?assertEqual("999", pmm_conf:get(country_code)),
    pmm_conf:stop(),
    wait_for_exit(PmmConfPid)
.

test3_get_config_changed_fun_test() ->
    EmptyConf = [],
    FullConf = [{a, 3}, {b, 6}, {c, 9}],
    ChangedConf = [{a, 3}, {b, 7}, {c, 10}],
    PartlyConf = [{a, 3}, {c, 9}],
    ExtendedConf = [{a, 3}, {b, 6}, {c, 9}, {d, 12}],
    ?assertEqual(pmm_conf:get_changed_settings(EmptyConf, FullConf), 
		 {[], [], [a, b, c]}),
    ?assertEqual(pmm_conf:get_changed_settings(FullConf, EmptyConf),
		 {[], [a, b, c], []}),
    ?assertEqual(pmm_conf:get_changed_settings(FullConf, ChangedConf),
		 {[b, c], [], []}),
    ?assertEqual(pmm_conf:get_changed_settings(FullConf, PartlyConf),
		 {[], [b], []}),
    ?assertEqual(pmm_conf:get_changed_settings(FullConf, ExtendedConf),
		 {[], [], [d]}),
    ?assertEqual(pmm_conf:get_changed_settings(ChangedConf, ExtendedConf),
		 {[b, c], [], [d]}).

set_up_config(LocalSettings) ->
    LogFun = fun(Str, Params) -> io:format(Str, Params) end,
    {ok, Pid} = pmm_conf:start_link({funnel, [], LogFun, LogFun},LocalSettings),
    pmm_conf:temporary_set(strip_leading_zero, false),
    pmm_conf:temporary_set(country_code, "999"),
    Pid.

wait_for_exit(Pid) ->
    MRef = erlang:monitor(process, Pid),
    receive {'DOWN', MRef, _, _, _} -> ok end.
