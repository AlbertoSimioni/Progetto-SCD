//classe che rappresenta la linea di separazione tra corsie
MyLine.prototype.constructor = MyLine;

//costruttore
function MyLine(from, to) {
    "use strict";
    this.from = from;
    this.to = to;
    this.path = null;
}

//campo dati statico
MyLine.width = 1;

//Disegna la linea
MyLine.prototype.draw = function() {
    this.path = new Path.Rectangle(this.from, this.to);
    this.path.fillColor = "white";
}