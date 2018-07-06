%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2018, Fred Youhanaie
%%% @doc
%%% run the EUnit tests for the mtxmkt module
%%% @end
%%% Created :  6 Jul 2018 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(mtxmkt_tests).

-include_lib("eunit/include/eunit.hrl").

-define(File_nofile, "test/nofile.mtx"). %% this file should not exist!
-define(File_nulldev, "/dev/null"). %% will work in Unix, not sure about windows!

%%--------------------------------------------------------------------
%% The tests
%%--------------------------------------------------------------------

% non-existent file
nofile_open_test() ->
    ?assertMatch({error, enoent, _Msg}, mtxmkt:mm_openread(?File_nofile)).

% empty file can be opened
emptyfile_open_test() ->
    IOdev = mtxmkt:mm_openread(?File_nulldev),
    ?assert(is_pid(IOdev)),
    file:close(IOdev).
