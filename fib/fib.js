console.time("WASM fib");

const fs = require('fs');
var source = fs.readFileSync('./fib.wasm');

var typedArray = new Uint8Array(source);
 
WebAssembly.instantiate(typedArray).then(result => {
  console.log(result.instance.exports.fib(20));
});

console.timeEnd("WASM fib");