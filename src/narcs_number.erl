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

%% @doc Encoder combinators that write numbers into big or little
%% endian bytes.

-module(narcs_number).


-export([f/2]).
-export([i/2]).
-export([u/2]).
-export_type([endian/0]).


-type endian() :: big | little.


-spec i(endian(), pos_integer()) -> narcs:encoder(integer(), binary()).

%% @doc Write a signed integer into a little or big endian bits.

i(Endianess, Size) -> int(Endianess, signed, Size).


-spec u(endian(), pos_integer()) -> narcs:encoder(non_neg_integer(), binary()).

%% @doc Write a unsigned integer into a little or big endian bits.

u(Endianess, Size) -> int(Endianess, unsigned, Size).


int(little, signed, Size) ->
    fun
        (Input) ->
            <<Input:Size/little-signed>>
    end;

int(little, unsigned, Size) ->
    fun
        (Input) ->
            <<Input:Size/little>>
    end;

int(big, signed, Size) ->
    fun
        (Input) ->
            <<Input:Size/signed>>
    end;

int(big, unsigned, Size) ->
    fun
        (Input) ->
            <<Input:Size>>
    end.


-spec f(endian(), pos_integer()) -> narcs:encoder(float(), binary()).

%% @doc Write a float into a little or big endian bits.

f(little, Size) ->
    fun
        (Input) ->
            <<Input:Size/float-little>>
    end;

f(big, Size) ->
    fun
        (Input) ->
            <<Input:Size/float>>
    end.
