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

%% @doc Encoder combinators that operate on sequences of output.


-module(narcs_sequence).


-export([sequence/1]).
-export([sequence/2]).


-spec sequence([narcs:encoder()]) -> narcs:encoder().

%% @doc Encode each step in sequence.

sequence(Steps) ->
    fun
        (Decoded) when is_map(Decoded) ->
            [Step(Decoded) || Step <- Steps]
    end.


-spec sequence(narcs:encoder(), narcs:encoder()) -> narcs:encoder().

%% @doc Output a map, encoding each key and value.

sequence(KeyEncoder, ValueEncoder) ->
    fun
        (Decoded) when is_map(Decoded) ->
            maps:fold(
              fun
                  (Key, Value, A) ->
                      [KeyEncoder(Key), ValueEncoder(Value) | A]
              end,
              [],
              Decoded)
    end.
