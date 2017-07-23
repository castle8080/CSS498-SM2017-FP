
function canI() {
    console.log("I think you can.\n");
    return true;
}

function shouldI() {
    console.log("Probably not.\n");
    return false;
}

if (shouldI() && canI()) {
    console.log("Do it!");
}






