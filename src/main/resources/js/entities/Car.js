Car.prototype = new Entity();
Car.prototype.constructor = Car;

function Car(id, lat,long, direction) {
    "use strict";
    this.id = id;
    this.direction = direction;
    if(this.direction == "left" || this.direction == "right")
        this.isHorizontal = true;
    else
        this.isHorizontal = false;
    /*
    if(this.isHorizontal){
        this.currentLat = lat + car.length/2.0;
        if(curStreet.isTripleLane()){
            var lane = "";
            if(curStreet.getTramPosition() == "up" && this.direction == "left") lane = "center";
            if(curStreet.getTramPosition() == "up" && this.direction == "right") lane = "down";
            if(curStreet.getTramPosition() == "down" && this.direction == "left") lane = "up";
            if(curStreet.getTramPosition() == "down" && this.direction == "right") lane = "center";
            //CODE TO CHANGE THE LONG
        }
        else{
            if(this.direction == "left") //UP
            else //DOWN
        }
    }
    else{
        this.currentLong = long + car.length/2.0
        if(curStreet.isTripleLane()){
            var lane = "";
            if(curStreet.getTramPosition() == "left" && this.direction == "up") lane = "right";
            if(curStreet.getTramPosition() == "left" && this.direction == "down") lane = "center";
            if(curStreet.getTramPosition() == "right" && this.direction == "up") lane = "center";
            if(curStreet.getTramPosition() == "right" && this.direction == "down") lane = "left";
            //CODE TO CHANGE THE LONG
        }
        else{
            if(this.direction == "up") //right
            else //left
        }
    }*/
    var size = null;
    var currLat = 0;
    var currLong = 0;
    if(this.isHorizontal){
        size = new Size(Car.leng,Car.height);
        currLat = lat + Car.leng/2.0;
        currLong = long + Car.height/2.0;
        }
    else{
        size = new Size(Car.height,Car.leng);
        currLat = lat + Car.height/2.0;
        currLong = long + Car.leng/2.0;
        }
    this.currentLat = currLat;
    this.currentLong = currLong;
    this.shape = new Shape.Rectangle(new Point(this.currentLat, this.currentLong),size);
    this.shape.fillColor = new Color(0.1, 0.85, 0.2);
};
//simula un campo statico
Car.leng = 3;
Car.height = 4;





//pos è di tipo int, e rappresenta la posizione dell'estremo della macchina, non il centro
Car.prototype.move = function(lat,long, direction) {
    "use strict";
    this.direction = direction;
    var isHor = false;
    if(this.direction == "left" || this.direction == "right")
        isHor = true;
    var currLat = 0;
    var currLong = 0;
    if(isHor){
        currLat = lat + Car.leng/2.0;
        currLong = long + Car.height/2.0;
        }
    else{
        currLat = lat + Car.height/2.0;
        currLong = long + Car.leng/2.0;
        }
    this.currentLat = currLat;
    this.currentLong = currLong;
    this.shape.position = new Point(this.currentLat,this.currentLong);
    if(this.isHorizontal !== isHor){ this.shape.rotate(90);}
    this.isHorizontal = isHor;
};



Car.prototype.draw = function() {
    "use strict";
    // calling the "super" method
    Entity.prototype.draw.call(this);
};