# Filbert ([@FilbertFs](https://twitter.com/FilbertFs))

## Introduction

Filbert is a simple **BERT** serializer and **BERT-RPC** client for .Net, written in F#.

## What is BERT?

[BERT] (http://bert-rpc.org/) is a flexible binary serialization format using the same encoding format as Erlang's [external term format] (http://erlang.org/doc/apps/erts/erl_ext_dist.html) but supports only the following data types:

* **SMALL_INTEGER_EXT** (tag 97) : unsigned 8-bit integer.
* **INTEGER_EXT** (tag 98) : signed 32-bit integer in big-endian format (MSB first).
* **FLOAT_EXT** (tag 99) : a float stored in scientific string format, e.g. .ToString("e20").
* **ATOM_EXT** (tag 100) : an Erlang atom, the maximum allowed length for atom is 255.
* **SMALL_TUPLE_EXT** (tag 104) : a tuple with up to 255 elements.
* **LARGE_TUPLE_EXT** (tag 105) : same as SMALL_TUPLE_EXT but supports up to 4294967295 (uint.MaxValue) elements.
* **NIL_EXT** (tag 106) : represents an empty list.
* **STRING_EXT** (tag 107) : string does NOT have a corresponding Erlang representation, but is an optimization for sending lists of bytes, the maximum number of bytes in the string is 65534.
* **LIST_EXT** (tag 108) : a list of elements of any type specified in this list, supports up to 4294967295 elements. For the sake of simplicity, __Filbert__ does not support improper lists ([a|b]) right now.
* **BINARY_EXT** (tag 109) : supports up to 4294967295 bytes.
* **SMALL_BIG_EXT** (tag 110) : big integer up to +/- 255*256^254.
* **LARGE_BIG_EXT** (tag 111) : big integer up to +/- 4294967295*256^4294967294!

In addition, BERT also specifies a number of complex types as tuples whose first item is the atom **bert**:
* **nil** : { bert, nil }, the equivalent of null/nil in other languages. (Erlang associated the primitive __nil__ with the empty array [] only)
* **boolean** : { bert, true } or { bert, false } for __true__ and __false__ respectively.
* **dictionary** : { bert, dict, [{name, <<"Yan">>}, {nick, theburningmonk}] }, a dictionary is represented as a tuple with 3 items - the atoms __bert__ and __dict__ followed by a list of 2 item tuples each representing a key-value pair.
* **time** : { bert, time, 1255, 295581, 446228 }, equals to 1255 megaseconds (millions of secons) + 295581 seconds + 446228 microseconds (millionths of a second, or 10 ticks) since the Unix Epoch time (1970 Jan 1st).

## How does it work?
Please see the [wiki page] (https://github.com/theburningmonk/Filbert/wiki) for a quick tutorial on how to use __Filbert__.

## NuGet

Download and install **Filbert** using [NuGet](https://nuget.org/packages/Filbert).

<a href="https://nuget.org/packages/Filbert"><img src="http://theburningmonk.com/images/filbert-nuget-install.png" alt="NuGet package"/></a>
