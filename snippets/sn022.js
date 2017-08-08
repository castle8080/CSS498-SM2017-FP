
function make_lines(n) {
    var lines = [];
    for (var i = 0; i < n; i++) {
        lines.push("Line " + i);
    }
    return lines;
}

function countif(items, f) {
    var count = 0;
    for (var i = 0, len = items.length; i < len; i++) {
        if (f(items[i])) 
            count++;
    }
    return count;
}

function timeit(f) {
    var start = new Date();
    var r = f();
    var end = new Date();
    console.log("Result computed in " + (end - start) + "ms. Result = " + r);
}

var lines = make_lines(1000000);
timeit(() => countif(lines, item => /(99|foo|bar|baz)/.test(item)));



