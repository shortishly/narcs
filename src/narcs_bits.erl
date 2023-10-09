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


%% @doc Encoder combinators for bit output.

-module(narcs_bits).


-export([into_bit/0]).
-export([into_bit/1]).
-export([into_bitfield/1]).


%% @doc Given a map of flags, output a bitstring ordered using the
%% supplied names.

-spec into_bitfield(#{A => boolean()}) -> narcs:encoder([A], bitstring()).

into_bitfield(Flags) ->
    fun
        (Names) ->
            list_to_bitstring(
              lists:map(
                fun
                    (Name) ->
                        (into_bit())(maps:get(Name, Flags, false))
                end,
                Names))
    end.


%% @doc Output a bit from a boolean value.

-spec into_bit() -> narcs:encoder(boolean(), <<_:1>>).

into_bit() ->
    fun
        (true) ->
            <<1:1>>;

        (false) ->
            <<0:1>>
    end.


-spec into_bit(narcs:encoder(A, boolean())) -> narcs:encoder(A, <<_:1>>).

into_bit(Encoder) ->
    fun
        (Decoded) ->
            (into_bit())(Encoder(Decoded))
    end.
