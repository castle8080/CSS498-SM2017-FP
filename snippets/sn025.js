
function add2(a, b) { return a + b; }

function add2(a) {
    return function(b) {
      return a + b;
    }
}

var add2 = (a) => (b) => a + b;

