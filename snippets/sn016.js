
function create_greeter(format) {
    var parts = format.split("#{name}");
    function greeter(name) {
        return parts.join(name);
    };
    return greeter;
}



