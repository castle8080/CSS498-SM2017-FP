
function CreateDispatcher(methods) {

    var dispatcher = function(message) {
        var args = Array.prototype.slice.call(arguments, 1);
        if (message in methods) {
            return methods[message].apply(dispatcher, args);
	}
        else {
            throw Error("Unknown method " + message + "!");
	}
    }

    return dispatcher;
}



function CreatePet(_name, _age) {
    return CreateDispatcher({
        name: () => _name,
        age: () => _age,
        eat: function(food) {
            return this("name") + " likes " + food + "!";
	}
    });
}

    function dispatcher(message) {
        var method;
        if (message == "name")
            method = name;
        else if (message == "age")
            method = age; 
        else if (message == "eat")
            method = eat; 
        else
            throw Error("Unkown method!");

        return method.apply(null, Array.prototype.slice.call(arguments, 1));
    }

    return dispatcher;
}



