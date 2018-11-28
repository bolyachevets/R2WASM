console.time("JS fib");

function fib(n) {
  if (n < 2){
    return 1
  }
  return fib(n - 1) + fib (n - 2)
}

console.log(fib(20));

console.timeEnd("JS fib");