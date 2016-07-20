
Zone.prototype.constructor = Zone;


//Costruttore
function Zone(id, from, type) {
    "use strict";
    this.id = id;
    this.from = from;
    this.to = new Point(from.x + Zone.width, from.y - Zone.width);
    this.type = type;
    this.path = null;
}


Zone.width = 12;


Zone.prototype.draw = function(){
    "use strict";
    console.log(this.from);
    console.log(this.to);
    this.path = new Path.Rectangle(this.from,this.to);
    if(this.type == "houseplace") this.path.fillColor = "yellow";
    if(this.type == "workplace") this.path.fillColor = "purple";
    if(this.type == "funplace") this.path.fillColor = "blue";
};