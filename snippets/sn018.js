
function CreatePet(name, age) {
    var self = {
      name: function() { return name; },
      age:  function() { return age; },
      describe: function() {
        return name + " is " + age + " years old and says " + self.speak() + ".";
      }
    };
    return self;
}

function CreateCat(name, age) {
    var self = CreatePet(name, age);
    self.speak = function() { return "Meow"; };
    return self;
}

function CreateDog(name, age) {
    var self = CreatePet(name, age);
    self.speak = function() { return "Woof"; };
    return self;
}

var pets = [CreateDog("Baily", 4), CreateCat("Gnome", 1)];

pets.forEach(function(pet) { console.log(pet.describe()) });


