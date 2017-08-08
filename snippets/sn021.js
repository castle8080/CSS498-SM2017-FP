
// Module A
A = (function() {
    function times2(x) {
        return x * 2;
    }
    function run() {
        console.log("times2(4) = " + times2(4));
    }
    return { run: run };
})();

