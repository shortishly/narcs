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

%% @doc Encoder combinators that deal with byte output.

-module(narcs_bytes).


-export([from_atom/0]).
-export([length_encoded/1]).
-export([null_terminated/0]).
-export([tag/1]).
-include_lib("kernel/include/logger.hrl").


%% @doc Use the supplied encoder to write the length of output.

-spec length_encoded(narcs:encoder()) -> narcs:encoder(iodata(), iodata()).

length_encoded(Encoder) ->
    fun
        (Decoded) ->
            ?LOG_DEBUG(#{encoder => Encoder, decoded => Decoded}),
            [Encoder(iolist_size(Decoded)), Decoded]
    end.


-spec null_terminated() -> narcs:encoder(iodata(), iodata()).

%% @doc Write a null byte after the output.

null_terminated() ->
    fun
        (Decoded) ->
            [Decoded, 0]
    end.


-spec from_atom() -> narcs:encoder(atom(), binary()).

%% @doc Write an atom as a sequence of bytes.

from_atom() ->
    fun
        (Decoded) ->
            erlang:atom_to_binary(Decoded)
    end.


-spec tag(binary()) -> narcs:encoder(any(), binary()).

%% @doc Write the matching output.

tag(Tag) ->
    fun
        (_) ->
            Tag
    end.
