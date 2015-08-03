Pedestrian.prototype = new Entity();
Pedestrian.prototype.constructor = Pedestrian;

function Pedestrian(id,position)
{
	this.id = id;
	this.shape = null;
	this.currentPosition = position;
}

Pedestrian.radius = 8;


// da cambiare con pos considerato come int
Pedestrian.prototype.move = function(pos){
	this.currentPosition = pos;
    this.shape.position = new Point(pos,39);
}

Pedestrian.prototype.draw = function(style)
{
    var center = new Point(this.currentPosition, 39);
	this.shape = new Shape.Circle(center,Pedestrian.radius);
    this.shape.fillColor = new Color(102, 205, 170);
        // calling the "super" method
    Entity.prototype.draw.call(this);
}