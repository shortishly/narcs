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

%% @doc Encoder combinators applying their child encoder multiple times.

-module(narcs_multi).

-export([count/2]).
-export([count/3]).
-export([count/4]).


-spec count(narcs:encoder(), narcs:encoder()) -> narcs:encoder().

%% @doc Write the encoded length of items, followed by each encoded
%% item.

count(NumOfItemEncoder, ItemEncoder) ->
    fun
        (Items) ->
            [NumOfItemEncoder(length(Items)),
             [ItemEncoder(Item) || Item <- Items]]
    end.


count(NumOfItemEncoder, KeyEncoder, ValueEncoder) ->
    ?FUNCTION_NAME(NumOfItemEncoder, undefined, KeyEncoder, ValueEncoder).


count(NumOfItemEncoder, IteratorOrder, KeyEncoder, ValueEncoder) ->
    fun
        (Items) ->
            [NumOfItemEncoder(maps:size(Items)),
             maps:fold(
               fun
                   (Key, Value, A) ->
                       [KeyEncoder(Key), ValueEncoder(Value) | A]
               end,
               [],
               maps:iterator(Items, IteratorOrder))]
    end.
