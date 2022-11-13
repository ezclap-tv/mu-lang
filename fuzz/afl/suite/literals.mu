let v = null;

let v = 0;
let v = 0.0;

let v = true;

let v = "test";
let v = "formatted {v}"; // string formatting
let v = "escaped curlies \{v}"; // exact value is `escaped curlies {v}`
let v = "\x2800";

let v = (v,);
let v = (v, v);

let v = [];
let v = [v, v, v, v];
let v = [v; 1024];
let v = v[0..512];

// inferred type
let name = 0;

// explicit type
// this declaration shadows the previous one, so its type may differ
let name: string = "value";
