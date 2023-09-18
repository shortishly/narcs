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


-module(narcs_bits_tests).


-import(narcs_tests, [t/2]).
-include_lib("eunit/include/eunit.hrl").


into_bitfield_test_() ->
    t(narcs_bits:into_bitfield(#{p => true, q => false, r => true}),
      [{<<2#101:3>>, [p, q, r]},
       {<<2#1010:4>>, [p, q, r, s]},
       {<<2#0101:4>>, [q, r, s, p]},
       {<<>>, []}]).


into_bit_test_() ->
    t(narcs_bits:into_bit(),
      [{<<1:1>>, true},
       {<<0:1>>, false}]).
