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

-define(File_nofile, "test/data/nofile.mtx"). %% this file should not exist!
-define(File_nulldev, "/dev/null"). %% will work in Unix, not sure about windows!
-define(File_non_mm, "test/data/testfile-non-mm.mtx").
-define(File_short_banner, "test/data/testfile-short-banner.mtx").
-define(File_extra_banner, "test/data/testfile-extra-banner.mtx").
-define(File_invalid_object, "test/data/testfile-invalid-object.mtx").
-define(File_invalid_format, "test/data/testfile-invalid-format.mtx").
-define(File_invalid_datatype, "test/data/testfile-invalid-datatype.mtx").
-define(File_invalid_symmetry, "test/data/testfile-invalid-symmetry.mtx").
-define(File_valid_banner, "test/data/testfile-valid-banner.mtx").

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

% empty file has no banner
emptyfile_banner_test() ->
    IOdev = mtxmkt:mm_openread(?File_nulldev),
    ?assert(is_pid(IOdev)),
    ?assertMatch({error, eof, _Msg}, mtxmkt:mm_read_banner(IOdev)),
    file:close(IOdev).

% Not a matrix market file
nonmmfile_test() ->
    IOdev = mtxmkt:mm_openread(?File_non_mm),
    ?assert(is_pid(IOdev)),
    ?assertMatch({error, mm_no_header, _Msg}, mtxmkt:mm_read_banner(IOdev)),
    file:close(IOdev).

% missing banner fields
banner_missing_fields_test() ->
    IOdev = mtxmkt:mm_openread(?File_short_banner),
    ?assert(is_pid(IOdev)),
    ?assertMatch({error, mm_invalid_header, _Msg}, mtxmkt:mm_read_banner(IOdev)),
    file:close(IOdev).

% too many banner fields
banner_extra_fields_test() ->
    IOdev = mtxmkt:mm_openread(?File_extra_banner),
    ?assert(is_pid(IOdev)),
    ?assertMatch({error, mm_invalid_header, _Msg}, mtxmkt:mm_read_banner(IOdev)),
    file:close(IOdev).

% invalid banner field - object
banner_invalid_object_test() ->
    IOdev = mtxmkt:mm_openread(?File_invalid_object),
    ?assert(is_pid(IOdev)),
    ?assertMatch({error, mm_unsupported_type, _Msg}, mtxmkt:mm_read_banner(IOdev)),
    file:close(IOdev).

% invalid banner field - format
banner_invalid_format_test() ->
    IOdev = mtxmkt:mm_openread(?File_invalid_format),
    ?assert(is_pid(IOdev)),
    ?assertMatch({error, mm_unsupported_type, _Msg}, mtxmkt:mm_read_banner(IOdev)),
    file:close(IOdev).

% invalid banner field - data type
banner_invalid_datatype_test() ->
    IOdev = mtxmkt:mm_openread(?File_invalid_datatype),
    ?assert(is_pid(IOdev)),
    ?assertMatch({error, mm_unsupported_type, _Msg}, mtxmkt:mm_read_banner(IOdev)),
    file:close(IOdev).

% invalid banner field - symmetry
banner_invalid_symmetry_test() ->
    IOdev = mtxmkt:mm_openread(?File_invalid_symmetry),
    ?assert(is_pid(IOdev)),
    ?assertMatch({error, mm_unsupported_type, _Msg}, mtxmkt:mm_read_banner(IOdev)),
    file:close(IOdev).

% valid banner
banner_valid_banner_test() ->
    IOdev = mtxmkt:mm_openread(?File_valid_banner),
    ?assert(is_pid(IOdev)),
    ?assertMatch({matrix, array, real, general}, mtxmkt:mm_read_banner(IOdev)),
    file:close(IOdev).
