Tram.prototype = new Entity();
Tram.prototype.constructor = Tram;

function Tram(id, lat,long, direction) {
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
        size = new Size(Tram.leng,Tram.height);
        currLat = lat + Tram.leng/2.0;
        currLong = long + Tram.height/2.0;
        }
    else{
        size = new Size(Tram.height,Tram.leng);
        currLat = lat + Tram.height/2;
        currLong = long + Tram.leng/2;
        }
    this.currentLat = currLat;
    this.currentLong = currLong;
    console.log(Tram.leng);
    console.log(size);
    this.shape = new Shape.Rectangle(new Point(this.currentLat, this.currentLong),size);
    this.shape.fillColor = new Color(0.33, 0.33, 0.33);
};


Tram.leng = 10;
Tram.height = 4;



Tram.prototype.move = function(lat,long, direction) {
    "use strict";
    this.direction = direction;
    var isHor = false;
    if(this.direction == "left" || this.direction == "right")
        isHor = true;
    var currLat = 0;
    var currLong = 0;
    if(isHor){
        currLat = lat + Tram.leng/2.0;
        currLong = long + Tram.height/2.0;
        }
    else{
        currLat = lat + Tram.height/2.0;
        currLong = long + Tram.leng/2.0;
        }
    this.currentLat = currLat;
    this.currentLong = currLong;
    this.shape.position = new Point(this.currentLat,this.currentLong);
    if(this.isHorizontal !== isHor){ this.shape.rotate(90);}
    this.isHorizontal = isHor;
   // this.shape.fillColor = new Color(123, 122, 122);
};



Tram.prototype.draw = function() {
    "use strict";
    // calling the "super" method
    Entity.prototype.draw.call(this);
};