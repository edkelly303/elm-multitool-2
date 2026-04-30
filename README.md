# elm-ir

## What?

Convert between Elm data types and an intermediate representation (IR)

## Why?

Because it makes it relatively easy to build cool tools (JSON encoders/decoders,
fuzzers, random generators, diff/patchers, parser/printers, and any other
bidirectional converters) for any Elm data type, with minimal boilerplate.

## How?

```elm
import IR
import IR.Fuzz
import IR.Json
import Fuzz
import Json.Decode

-- For a data type like this:

type alias User = 
    { name : String
    , age : Int 
    }

input = 
    { name = "Ed"
    , age = 44 
    }

-- Just write an IR.Codec like this:

codec = 
    IR.succeed User 
        |> IR.andMap .name IR.string 
        |> IR.andMap .age IR.int

-- Now you can convert to and from IR like this:

ir = 
    IR.fromInput codec input

ir --> IR.Product [ IR.Int 44, IR.String "Ed" ] 

output = 
    IR.toOutput codec ir

output --> Ok input

-- Now we just need to write a JSON encoder and 
-- decoder for our IR type, and we'll be able to 
-- use our Codec to convert our User type to and 
-- from JSON. Here's a JSON IR adapter I made earlier:

json = 
    IR.Json.encode codec input

json --: Json.Decode.Value

decoded = 
    Json.Decode.decodeValue (IR.Json.decoder codec) json

decoded --> Ok input

-- The best part is, we can use the exact same Codec 
-- for other things too. Say we want a fuzzer 
-- for testing: we just write a fuzzer for IR and we 
-- can use it with any Codec:

fuzzed = 
    Fuzz.examples 1 (IR.Fuzz.fuzzer codec)

fuzzed --> [ { name = "", age = 105 } ]
```