console.time("WASM fact");

const fs = require('fs');
var source = fs.readFileSync('./fact.wasm');

var typedArray = new Uint8Array(source);
 
WebAssembly.instantiate(typedArray).then(result => {
  console.log(result.instance.exports.fact(10));
});

console.timeEnd("WASM fact");