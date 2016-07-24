Bus.prototype = new Entity();
Bus.prototype.constructor = Bus;

function Bus(id, lat,long, direction) {
    "use strict";
    this.id = id;
    this.direction = direction;
    if(this.direction == "left" || this.direction == "right")
        this.isHorizontal = true;
    else
        this.isHorizontal = false;
    var size = null;
    var currLat = 0;
    var currLong = 0;
    if(this.isHorizontal){
        size = new Size(Bus.leng,Bus.height);
        currLat = lat + Bus.leng/2.0;
        currLong = long + Bus.height/2.0;
        }
    else{
        size = new Size(Bus.height,Bus.leng);
        currLat = lat + Bus.height/2.0;
        currLong = long + Bus.leng/2.0;
        }
    this.currentLat = currLat;
    this.currentLong = currLong;
    this.shape = new Shape.Rectangle(new Point(this.currentLat, this.currentLong),size);
    this.shape.fillColor = new Color(1, 0, 0);
};

Bus.leng = 5;
Bus.height = 4;



Bus.prototype.move = function(lat,long, direction) {
    "use strict";
    this.direction = direction;
    var isHor = false;
    if(this.direction == "left" || this.direction == "right")
        isHor = true;
    var currLat = 0;
    var currLong = 0;
    if(isHor){
        currLat = lat + Bus.leng/2.0;
        currLong = long + Bus.height/2.0;
        }
    else{
        currLat = lat + Bus.height/2.0;
        currLong = long + Bus.leng/2.0;
        }
    this.currentLat = currLat;
    this.currentLong = currLong;
    this.shape.position = new Point(this.currentLat,this.currentLong);
    if(this.isHorizontal !== isHor){ this.shape.rotate(90);}
    this.isHorizontal = isHor;
};



Bus.prototype.draw = function() {
    "use strict";
    // calling the "super" method
    Entity.prototype.draw.call(this);
};