const fs = require('fs');
var source = fs.readFileSync('./add.wasm');

var typedArray = new Uint8Array(source);

WebAssembly.instantiate(typedArray).then(result => {
  console.log(result.instance.exports.add(1, 2));
});