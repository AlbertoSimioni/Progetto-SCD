//classe che rappresenta la linea di separazione tra corsie
Sidewalk.prototype.constructor = Sidewalk;

//costruttore
function Sidewalk(from, to) {
    "use strict";
    this.from = from;
    this.to = to;
    this.path = null;
}

//campo dati statico
Sidewalk.width = 2;
Sidewalk.color = "#616360";


Sidewalk.prototype.changeColor = function(color){
    if(color == "oldColor") this.path.fillColor = Sidewalk.color;
    else this.path.fillColor = color;
}


//Disegna la linea
Sidewalk.prototype.draw = function() {
    this.path = new Path.Rectangle(this.from, this.to);
    this.path.fillColor = Sidewalk.color;
}