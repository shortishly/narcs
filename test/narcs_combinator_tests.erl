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


-module(narcs_combinator_tests).


-import(narcs_tests, [t/2]).
-include_lib("eunit/include/eunit.hrl").


map_result_test_() ->
    t(narcs_result:to_binary(
        narcs_combinator:map_result(
          narcs_bytes:length_encoded(
            narcs_number:u(big, 16)),
          fun
              (Result) ->
                  ["pqr", Result]
          end)),
      [{<<"pqr", 3:16, "abc">>, <<"abc">>},
       {<<"pqr", 0:16>>, <<>>}]).


v_test_() ->
    t(narcs_result:to_binary(
        narcs_combinator:v(
          everything,
          narcs_number:u(big, 16))),
      [{<<42:16>>, #{everything => 42}}]).


condition_test_() ->
    t(narcs_result:to_binary(
        narcs_combinator:condition(
          pqr,
          narcs_combinator:v(
            everything,
            narcs_number:u(big, 16)))),
      [{<<42:16>>, #{pqr => true, everything => 42}},
       {<<>>, #{pqr => false, everything => 42}}]).
