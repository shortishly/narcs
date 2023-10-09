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


-module(narcs_bytes_tests).


-include_lib("eunit/include/eunit.hrl").
-import(narcs_tests, [t/2]).


length_encoded_test_() ->
    t(narcs_result:to_binary(
        narcs_bytes:length_encoded(narcs_number:u(big, 16))),
      [{<<3:16, "abc">>, <<"abc">>},
       {<<0:16>>, <<>>}]).


null_terminated_test_() ->
    t(narcs_result:to_binary(
        narcs_bytes:null_terminated()),
      [{<<"abc", 0>>, <<"abc">>},
       {<<0>>, <<>>}]).


from_atom_test_() ->
    t(narcs_result:to_binary(
        narcs_bytes:from_atom()),
      [{<<"abc">>, abc}]).

take_n_test_() ->
    t(narcs_bytes:take(6),
      [{<<"123456">>, <<"123456">>}]).
