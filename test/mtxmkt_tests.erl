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
-define(File_invalid_size_crd, "test/data/testfile-size-invalid-crd.mtx").
-define(File_invalid_size_crd_2, "test/data/testfile-size-invalid-crd-2.mtx").
-define(File_invalid_size_array, "test/data/testfile-size-invalid-array.mtx").
-define(File_invalid_size_array_2, "test/data/testfile-size-invalid-array-2.mtx").
-define(File_valid_size_crd, "test/data/testfile-size-valid-crd.mtx").
-define(File_valid_size_array, "test/data/testfile-size-valid-array.mtx").
-define(File_short_crd_patt_gen, "test/data/testfile-short-crd-patt-gen.mtx").
-define(File_invalid_crd_patt_gen, "test/data/testfile-invalid-crd-patt-gen.mtx").

-define(File_valid_crd_patt_gen, "test/data/testfile-valid-crd-patt-gen.mtx").
-define(File_valid_crd_int_gen, "test/data/testfile-valid-crd-int-gen.mtx").
-define(File_valid_crd_real_gen, "test/data/testfile-valid-crd-real-gen.mtx").
-define(File_valid_crd_complex_gen, "test/data/testfile-valid-crd-complex-gen.mtx").

-define(File_valid_array_int_gen, "test/data/testfile-valid-array-int-gen.mtx").
-define(File_valid_array_real_gen, "test/data/testfile-valid-array-real-gen.mtx").
-define(File_valid_array_complex_gen, "test/data/testfile-valid-array-complex-gen.mtx").

-define(File_valid_array_int_symm, "test/data/testfile-valid-array-int-symm.mtx").
-define(File_valid_array_real_symm, "test/data/testfile-valid-array-real-symm.mtx").
-define(File_valid_array_complex_symm, "test/data/testfile-valid-array-complex-symm.mtx").

-define(File_valid_crd_patt_symm, "test/data/testfile-valid-crd-patt-symm.mtx").
-define(File_valid_crd_int_symm, "test/data/testfile-valid-crd-int-symm.mtx").
-define(File_valid_crd_real_symm, "test/data/testfile-valid-crd-real-symm.mtx").
-define(File_valid_crd_complex_symm, "test/data/testfile-valid-crd-complex-symm.mtx").

-define(File_valid_crd_complex_herm, "test/data/testfile-valid-crd-complex-herm.mtx").

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
    ?assertMatch({array, real, general}, mtxmkt:mm_read_banner(IOdev)),
    file:close(IOdev).

% missing sizes coordinate format
mising_size_array_test() ->
    IOdev = mtxmkt:mm_openread(?File_valid_banner),
    ?assert(is_pid(IOdev)),
    ?assertMatch({array, real, general}, mtxmkt:mm_read_banner(IOdev)),
    ?assertMatch({error, eof, _Msg}, mtxmkt:mm_read_mtx_array_size(IOdev)),
    file:close(IOdev).

% missing sizes array format
missing_size_crd_test() ->
    IOdev = mtxmkt:mm_openread(?File_valid_banner),
    ?assert(is_pid(IOdev)),
    ?assertMatch({array, real, general}, mtxmkt:mm_read_banner(IOdev)),
    ?assertMatch({error, eof, _Msg}, mtxmkt:mm_read_mtx_crd_size(IOdev)),
    file:close(IOdev).

% invalid sizes coordinate format
invalid_size_crd_test() ->
    IOdev = mtxmkt:mm_openread(?File_invalid_size_crd),
    ?assert(is_pid(IOdev)),
    ?assertMatch({coordinate, real, general}, mtxmkt:mm_read_banner(IOdev)),
    ?assertMatch({error, mm_invalid_size, _Msg}, mtxmkt:mm_read_mtx_crd_size(IOdev)),
    file:close(IOdev).

% invalid sizes coordinate format
invalid_size_crd_2_test() ->
    IOdev = mtxmkt:mm_openread(?File_invalid_size_crd_2),
    ?assert(is_pid(IOdev)),
    ?assertMatch({coordinate, real, general}, mtxmkt:mm_read_banner(IOdev)),
    ?assertMatch({error, mm_invalid_size, _Msg}, mtxmkt:mm_read_mtx_crd_size(IOdev)),
    file:close(IOdev).

% invalid sizes array format
invalid_size_array_test() ->
    IOdev = mtxmkt:mm_openread(?File_invalid_size_array),
    ?assert(is_pid(IOdev)),
    ?assertMatch({array, real, general}, mtxmkt:mm_read_banner(IOdev)),
    ?assertMatch({error, mm_invalid_size, _Msg}, mtxmkt:mm_read_mtx_array_size(IOdev)),
    file:close(IOdev).

% invalid sizes array format
invalid_size_array_2_test() ->
    IOdev = mtxmkt:mm_openread(?File_invalid_size_array_2),
    ?assert(is_pid(IOdev)),
    ?assertMatch({array, real, general}, mtxmkt:mm_read_banner(IOdev)),
    ?assertMatch({error, mm_invalid_size, _Msg}, mtxmkt:mm_read_mtx_array_size(IOdev)),
    file:close(IOdev).

% valid sizes coordinate format
valid_size_crd_test() ->
    IOdev = mtxmkt:mm_openread(?File_valid_size_crd),
    ?assert(is_pid(IOdev)),
    ?assertMatch({coordinate, real, general}, mtxmkt:mm_read_banner(IOdev)),
    ?assertMatch({5, 4, 12}, mtxmkt:mm_read_mtx_crd_size(IOdev)),
    file:close(IOdev).

% valid sizes array format
valid_size_array_test() ->
    IOdev = mtxmkt:mm_openread(?File_valid_size_array),
    ?assert(is_pid(IOdev)),
    ?assertMatch({array, real, general}, mtxmkt:mm_read_banner(IOdev)),
    ?assertMatch({5, 4}, mtxmkt:mm_read_mtx_array_size(IOdev)),
    file:close(IOdev).

% valid coordinate pattern general
valid_crd_patt_gen_test() ->
    {Mtx_code, M} = mtxmkt:mm_readfile(?File_valid_crd_patt_gen),
    ?assertMatch({{coordinate, pattern, general},
		  [
		   [1,0,1],
		   [0,1,0],
		   [1,0,1]
		  ]}, {Mtx_code, matrix:to_list(M)}).

% short coordinate pattern general
short_crd_patt_gen_test() ->
    ?assertMatch({error, eof, _Msg}, mtxmkt:mm_readfile(?File_short_crd_patt_gen)).

% short coordinate pattern general
invalid_crd_patt_gen_test() ->
    ?assertMatch({error, mm_invalid_entry, _Msg}, mtxmkt:mm_readfile(?File_invalid_crd_patt_gen)).

% valid coordinate integer general
valid_crd_int_gen_test() ->
    {Mtx_code, M} = mtxmkt:mm_readfile(?File_valid_crd_int_gen),
    ?assertMatch({{coordinate, integer, general},
		  [
		   [1,0,2],
		   [0,3,0],
		   [4,0,5]
		  ]}, {Mtx_code, matrix:to_list(M)}).

% valid coordinate real general
valid_crd_real_gen_test() ->
    {Mtx_code, M} = mtxmkt:mm_readfile(?File_valid_crd_real_gen),
    ?assertMatch({{coordinate, real, general},
		  [
		   [1.1,0.0,2.2],
		   [0.0,3.3,0.0],
		   [4.4,0.0,5.5]
		  ]}, {Mtx_code, matrix:to_list(M)}).

% valid coordinate complex general
valid_crd_complex_gen_test() ->
    {Mtx_code, M} = mtxmkt:mm_readfile(?File_valid_crd_complex_gen),
    ?assertMatch({{coordinate, complex, general},
		  [
		   [{1.1,1.2},{0.0,0.0},{2.2,2.3}],
		   [{0.0,0.0},{3.3,3.4},{0.0,0.0}],
		   [{4.4,4.5},{0.0,0.0},{5.5,5.6}]
		  ]}, {Mtx_code, matrix:to_list(M)}).

% valid array integer general
valid_array_int_gen_test() ->
    {Mtx_code, M} = mtxmkt:mm_readfile(?File_valid_array_int_gen),
    ?assertMatch({{array, integer, general},
		  [
		   [1,2,3],
		   [4,5,6],
		   [7,8,9]
		  ]}, {Mtx_code, matrix:to_list(M)}).

% valid array real general
valid_array_real_gen_test() ->
    {Mtx_code, M} = mtxmkt:mm_readfile(?File_valid_array_real_gen),
    ?assertMatch({{array, real, general},
		  [
		   [1.1,2.2,3.3],
		   [4.4,5.5,6.6],
		   [7.7,8.8,9.9]
		  ]}, {Mtx_code, matrix:to_list(M)}).

% valid coordinate complex general
valid_array_complex_gen_test() ->
    {Mtx_code, M} = mtxmkt:mm_readfile(?File_valid_array_complex_gen),
    ?assertMatch({{array, complex, general},
		  [
		   [{1.1,1.2}, {2.2,2.3}, {3.3,3.4}],
		   [{4.4,4.5}, {5.5,5.6}, {6.6,6.7}],
		   [{7.7,7.8}, {8.8,8.9}, {9.9,9.1}]
		  ]}, {Mtx_code, matrix:to_list(M)}).

% valid array integer symmetric
valid_array_int_symm_test() ->
    {Mtx_code, M} = mtxmkt:mm_readfile(?File_valid_array_int_symm),
    ?assertMatch({{array, integer, symmetric},
		  [
		   [11, 12, 13, 14],
		   [12, 15, 16, 17],
		   [13, 16, 18, 19],
		   [14, 17, 19, 20]
		  ]}, {Mtx_code, matrix:to_list(M)}).

% valid array real symmetric
valid_array_real_symm_test() ->
    {Mtx_code, M} = mtxmkt:mm_readfile(?File_valid_array_real_symm),
    ?assertMatch({{array, real, symmetric},
		  [
		   [1.1, 1.2, 1.3, 1.4],
		   [1.2, 1.5, 1.6, 1.7],
		   [1.3, 1.6, 1.8, 1.9],
		   [1.4, 1.7, 1.9, 2.0]
		  ]}, {Mtx_code, matrix:to_list(M)}).

% valid array complex symmetric
valid_array_complex_symm_test() ->
    {Mtx_code, M} = mtxmkt:mm_readfile(?File_valid_array_complex_symm),
    ?assertMatch({{array, complex, symmetric},
		  [
		   [{11.1, 11.2}, {12.2, 12.3}, {13.3, 13.4}, {14.4, 14.5}],
		   [{12.2, 12.3}, {15.5, 15.6}, {16.6, 16.7}, {17.7, 17.8}],
		   [{13.3, 13.4}, {16.6, 16.7}, {18.8, 18.9}, {19.9, 19.0}],
		   [{14.4, 14.5}, {17.7, 17.8}, {19.9, 19.0}, {20.0, 20.1}]
		  ]}, {Mtx_code, matrix:to_list(M)}).

% valid coordinate pattern symmetric
valid_crd_patt_symm_test() ->
    {Mtx_code, M} = mtxmkt:mm_readfile(?File_valid_crd_patt_symm),
    ?assertMatch({{coordinate, pattern, symmetric},
		  [
		   [1,0,1,0],
		   [0,1,0,1],
		   [1,0,1,0],
		   [0,1,0,1]
		  ]}, {Mtx_code, matrix:to_list(M)}).

% valid coordinate integer symmetric
valid_crd_int_symm_test() ->
    {Mtx_code, M} = mtxmkt:mm_readfile(?File_valid_crd_int_symm),
    ?assertMatch({{coordinate, integer, symmetric},
		  [
		   [1,0,2,0],
		   [0,3,0,4],
		   [2,0,5,0],
		   [0,4,0,6]
		  ]}, {Mtx_code, matrix:to_list(M)}).

% valid coordinate real symmetric
valid_crd_real_symm_test() ->
    {Mtx_code, M} = mtxmkt:mm_readfile(?File_valid_crd_real_symm),
    ?assertMatch({{coordinate, real, symmetric},
		  [
		   [1.1,0.0,4.4,2.2],
		   [0.0,3.3,0.0,0.0],
		   [4.4,0.0,5.5,0.0],
		   [2.2,0.0,0.0,6.6]
		  ]}, {Mtx_code, matrix:to_list(M)}).

% valid coordinate complex symmetric
valid_crd_complex_symm_test() ->
    {Mtx_code, M} = mtxmkt:mm_readfile(?File_valid_crd_complex_symm),
    ?assertMatch({{coordinate, complex, symmetric},
		  [
		   [{1.1,1.2},{0.0,0.0},{4.4,4.5},{2.2,2.3}],
		   [{0.0,0.0},{3.3,3.4},{0.0,0.0},{0.0,0.0}],
		   [{4.4,4.5},{0.0,0.0},{5.5,5.6},{0.0,0.0}],
		   [{2.2,2.3},{0.0,0.0},{0.0,0.0},{6.6,6.7}]
		  ]}, {Mtx_code, matrix:to_list(M)}).

% valid coordinate complex hermitian
valid_crd_complex_herm_test() ->
    {Mtx_code, M} = mtxmkt:mm_readfile(?File_valid_crd_complex_herm),
    ?assertMatch({{coordinate, complex, hermitian},
		  [
		   [{1.1,1.2},{0.0,0.0},{4.4,-4.5},{2.2, -2.3}],
		   [{0.0,0.0},{3.3,3.4},{0.0, 0.0},{0.0,  0.0}],
		   [{4.4,4.5},{0.0,0.0},{5.5, 5.6},{0.0,  0.0}],
		   [{2.2,2.3},{0.0,0.0},{0.0, 0.0},{6.6,  6.7}]
		  ]}, {Mtx_code, matrix:to_list(M)}).
