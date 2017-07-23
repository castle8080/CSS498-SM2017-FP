

function max(array) {
    var value = array[0];
    for (var i = 0, len = array.length; i < len; i++) {
        if (array[i] > value) {
            value = array[i];
        }
    }
    return value;
}

