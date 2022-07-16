v := null
v: null

v: int
v := 0 // int
v := 0i
v: float
v := 0.0 // float
v := 0f

v := true // bool
v: bool

v := "test" // string
v := "formatted {v}" // string interpolation
v := "escaped curlies \{v}" // exact value is `escaped curlies {v}`
// strings also support other escaped charaters such as `\n`, `\t`, `\x2800`, etc.
v := "\x2800"
v: string

// string and number literals can also be suffixed
// LITERALsuffix is transformed to suffix(LITERAL)
v := 10ms // this calls the `ms` function with the value `10`
v := "3f9a1c30-dd8f-4769-8ac7-214f94d7d70c"uuid // parses the string as a uuid

v := (v,) // tuple
v: (T,)
v := (v, v)
v: (T,T)

v := [] // array
v := [v, v, v, v]
v := [v; 1024]
v: [T]

v := { v: v, [v]: v, ...v } // record
v: { v: T }