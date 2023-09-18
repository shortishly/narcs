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


-module(narcs).


-export_type([encoder/0]).
-export_type([encoder/2]).
-export_type([input/0]).
-export_type([output/0]).


-type input() :: any().

-type output() :: iodata()
                | bitstring()
                | nomatch.

-type encoder(I, O) :: fun((I) -> O).

-type encoder() :: encoder(input(), output()).
