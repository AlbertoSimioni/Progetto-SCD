Car.prototype = new Entity();
Car.prototype.constructor = Car;

function Car(id, position) {
    "use strict";
    this.id = id;
    this.shape = null;
    this.rectangle = null;
    this.currentPosition = position;
    this.angle = 90;
};
//simula un campo stato
Car.length = 30;





//pos è di tipo int, e rappresenta la posizione dell'estremo della macchina, non il centro
Car.prototype.move = function(pos, angle) {
    "use strict";
    //var p = new Point();
    //p.length = this.length/2; //lunghezza del punto
    //p.angle = this.angle; //Angolo in gradi dall'origine al punto


    this.currentPosition = pos;
    this.shape.position = new Point(pos,15);
    this.shape.rotate(angle - this.angle);
   // this.shape.fillColor = new Color(123, 122, 122);
    this.angle = angle;
};



Car.prototype.draw = function() {
    "use strict";
    var point = new Point(this.currentPosition, 15)
    var size = new Size(20,Car.length)
    this.shape = new Shape.Rectangle(point,size);
    this.shape.fillColor = new Color(1, 0, 0);
    // calling the "super" method
    Entity.prototype.draw.call(this);
};