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


%% @doc Encoder combinators.

-module(narcs_combinator).


-export([condition/2]).
-export([map_result/2]).
-export([rest/0]).
-export([v/2]).
-include_lib("kernel/include/logger.hrl").


%% @doc Map the result of the supplied encoder.

map_result(Encoder, Mapping) ->
    fun
        (Decoded) ->
            (Mapping(Encoder(Decoded)))
    end.


%% @doc Apply the encoder if the condition is true.

-type condition() :: boolean()
                   | atom().

-spec condition(condition(), narcs:encoder()) -> narcs:encoder().

condition(true, Encoder) ->
    fun
        (Decoded) ->
            (Encoder)(Decoded)
    end;

condition(false, _) ->
    fun
        (_) ->
            <<>>
    end;

condition(Key, Encoder) ->
    fun
        (#{Key := true} = Decoded) ->
            (Encoder)(Decoded);

        (#{Key := false}) ->
            <<>>;

        (_) ->
            error(badarg, [Key, Encoder])
    end.


%% @doc Apply an encoder on the value represented by the supplied key.

-spec v(any(), narcs:encoder()) -> narcs:encoder(map(), narcs:output()).

v(Key, Encoder) ->
    fun
        (#{Key := Value} = Decoded) ->
            ?LOG_DEBUG(#{key => Key, value => Value, decoded => Decoded}),
            Encoder(Value);

        (_) ->
            error(badarg, [Key, Encoder])
    end.


%% @doc Use the rest of the output as is.

-spec rest() -> narcs:encoder().

rest() ->
    fun
        (Decoded) ->
            Decoded
    end.
