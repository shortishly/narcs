<br>

<p align="center">
    <a href="https://shortishly.github.io/narcs/cover/">
      <img alt="Test Coverage" src="https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fshortishly.github.io%2Fnarcs%2Fcover%2Fcoverage.json&query=%24.total&suffix=%25&style=flat-square&label=Test%20Coverage&color=green">
    </a>
    <a href="https://shortishly.github.io/narcs/edoc/">
      <img alt="edoc" src="https://img.shields.io/badge/Documentation-edoc-green?style=flat-square">
    </a>
    <a href="https://erlang.org/">
      <img alt="Erlang/OTP 25+" src="https://img.shields.io/badge/Erlang%2FOTP-25%2B-green?style=flat-square">
    </a>
    <a href="https://www.apache.org/licenses/LICENSE-2.0">
      <img alt="Apache-2.0" src="https://img.shields.io/github/license/shortishly/narcs?style=flat-square">
    </a>
</p>

## What is narcs?

narcs is an encoder combinator library, essential the reverse of the
decoder combinators provided by [scran][scran].

## Examples

For a 16 bit big endian unsigned length encoded string of "abc", returning iodata:

```erlang
1> (narcs_bytes:length_encoded(narcs_number:u(big, 16)))(<<"abc">>).
[<<0,3>>,<<"abc">>]
```

For the same string to be null terminated:

```erlang
1> (narcs_bytes:null_terminated())(<<"abc">>).
[<<"abc">>,0].
```

Using a map of parameters, to encode some `flags` (32 bit integer),
followed by some null terminated `bytes`:

```erlang
1> (narcs_sequence:sequence(
      [narcs_combinator:v(flags, narcs_number:u(big, 32)),
       narcs_combinator:v(bytes, narcs_bytes:null_terminated())]))
         (#{flags => 128, bytes => <<"hello world!">>}).
[<<0,0,0,128>>,[<<"hello world!">>,0]]
```

Using a map of parameters, write each key as null terminated bytes and
each value as a 16 bit numeric value:

```erlang
1> KV = (narcs_sequence:sequence(
         %% keys are atoms encoded as null terminated bytes:
         narcs_combinator:map_result(
             narcs_bytes:from_atom(),
             narcs_bytes:null_terminated()),

         %% values are 16 bit big endian:
         narcs_number:u(big, 16))).

#Fun<narcs_sequence.1.53547035>

2> KV(#{a => 1, b => 2, c => 3}).

[[<<"b">>,0],
 <<0,2>>,
 [<<"a">>,0],
 <<0,1>>,
 [<<"c">>,0],
 <<0,3>>]
```


[scran]: https://github.com/shortishly/scran
