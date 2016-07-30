Pedestrian.prototype = new Entity();
Pedestrian.prototype.constructor = Pedestrian;

function Pedestrian(id, lat,long, direction) {
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
        size = new Size(Pedestrian.leng,Pedestrian.height);
        currLat = lat + Pedestrian.leng/2.0;
        currLong = long + Pedestrian.height/2.0;
        }
    else{
        size = new Size(Pedestrian.height,Pedestrian.leng);
        currLat = lat + Pedestrian.height/2;
        currLong = long + Pedestrian.leng/2;
        }
    this.currentLat = currLat;
    this.currentLong = currLong;
    this.shape = new Shape.Rectangle(new Point(this.currentLat, this.currentLong),size);
    this.shape.fillColor = new Color(0.1, 0.85, 0.72);
    this.shape.myData = this.id;
};


Pedestrian.leng = 1;
Pedestrian.height = 1;



Pedestrian.prototype.move = function(lat,long, direction) {
    "use strict";
    this.direction = direction;
    var isHor = false;
    if(this.direction == "left" || this.direction == "right")
        isHor = true;
    var currLat = 0;
    var currLong = 0;
    if(isHor){
        currLat = lat + Pedestrian.leng/2.0;
        currLong = long + Pedestrian.height/2.0;
        }
    else{
        currLat = lat + Pedestrian.height/2.0;
        currLong = long + Pedestrian.leng/2.0;
        }
    this.currentLat = currLat;
    this.currentLong = currLong;
    this.shape.position = new Point(this.currentLat,this.currentLong);
    if(this.isHorizontal !== isHor){ this.shape.rotate(90);}
    this.isHorizontal = isHor;
   // this.shape.fillColor = new Color(123, 122, 122);
};



Pedestrian.prototype.draw = function() {
    "use strict";
    // calling the "super" method
    Entity.prototype.draw.call(this);
};