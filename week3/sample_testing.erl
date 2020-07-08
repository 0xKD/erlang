-module(sample_testing).
-export([double/1]).
-include_lib("eunit/include/eunit.hrl").

double(N) -> N * 2.

double_a_test() -> 4 = double(2).
double_b_test() -> ?assertEqual(double(32), 64).
double_c_test() -> ?assertEqual(double(1), 3).
