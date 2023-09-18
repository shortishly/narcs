%% Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-module(narcs_sequence_tests).


-import(narcs_tests, [t/2]).
-include_lib("eunit/include/eunit.hrl").


sequence_1_test_() ->
    t(narcs_sequence:sequence(
        [narcs_combinator:v(p, narcs_bits:into_bit()),
         narcs_combinator:v(q, narcs_number:u(big, 16))]),
      [{[<<1:1>>, <<5:16>>], #{p => true, q => 5}}]).


sequence_2_test_() ->
    t(narcs_combinator:map_result(
        narcs_sequence:sequence(
          narcs_combinator:map_result(
            narcs_bytes:from_atom(),
            narcs_bytes:null_terminated()),
          narcs_number:u(big, 16)),
        narcs_bytes:length_encoded(
          narcs_number:u(big, 16))),
      [{[<<12:16>>,
         [[<<"b">>,0], <<2:16>>,
          [<<"a">>,0], <<1:16>>,
          [<<"c">>,0], <<3:16>>]],
        #{a => 1, b => 2, c => 3}}]).
