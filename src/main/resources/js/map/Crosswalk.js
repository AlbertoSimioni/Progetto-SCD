//Classe che rappresenta una corsia stradale
Crosswalk.prototype.constructor = Crosswalk;


//Costruttore
function Crosswalk(id, from, isHorizontal, isTripleLane) {
    "use strict";
    this.id = id;
    this.from = from; //tipo point. punto in basso a sx
    this.to = new Point(this.from.x + Crosswalk.width, this.from.y - Crosswalk.width);
    var bonus = MyLine.width;
    if(isTripleLane) bonus +=  MyLine.width + Lane.width;
    if(isHorizontal) this.to.y -= bonus;
    else this.to.x +=bonus;
    this.isHorizontal = isHorizontal;
    this.path = null; //Non c'è un solo path, dovrei fare un array di path, per ora non mi serve
                      //il riferimento
    this.isTripleLane = isTripleLane;
}

Crosswalk.width = 12;
Crosswalk.linesWidth = 2;


Crosswalk.prototype.draw = function(){
    "use strict";
    if(this.isHorizontal) this.drawHorizontal();
    else this.drawVertical();
    /*
    this.path = new Path.Rectangle(this.from, this.to);
    this.path.fillColor = "#C0C1C4";
    */
};

//disegna le strisce su strade orizzontali, sia a corsie triple che doppie
Crosswalk.prototype.drawHorizontal = function(){
    "use strict";
    var xStart = this.from.x;
    var xEnd = this.to.x;
    var curY = this.from.y;
    var white = true;
    var numLines = 6;
    if(this.isTripleLane) numLines = 10;
    for(var i = 0; i < numLines; i++){
        var path = new Path.Rectangle(new Point(xStart,curY), new Point(xEnd, curY-Crosswalk.linesWidth));
        if(white){
            path.fillColor = "white";
        }
        else path.fillColor = "black";
        white = !white;
        curY -= Crosswalk.linesWidth;
    }
    if(!this.isTripleLane){
        var path0 = new Path.Rectangle(new Point(xStart,curY), new Point(xEnd, this.to.y));
        path0.fillColor = "white";
    }
    var path1 = new Path.Rectangle(new Point(xStart,this.from.y + Sidewalk.width), new Point(xEnd,this.from.y));
    path1.fillColor = Sidewalk.color;
    var path2 = new Path.Rectangle(new Point(xStart,this.to.y), new Point(xEnd,this.to.y - Sidewalk.width));
    path2.fillColor = Sidewalk.color;
};

//disegna le strisce su strade verticali, sia a corsie triple che doppie
Crosswalk.prototype.drawVertical = function(){
    "use strict";
    var yStart = this.from.y;
    var yEnd = this.to.y;
    var curX = this.from.x;
    var white = true;
    var numLines = 6;
    if(this.isTripleLane) numLines = 10;
    for(var i = 0; i < numLines; i++){
        var path = new Path.Rectangle(new Point(curX,yStart), new Point(curX+Crosswalk.linesWidth, yEnd));
        if(white){
            path.fillColor = "white";
        }
        else path.fillColor = "black";
        white = !white;
        curX += Crosswalk.linesWidth;
    }
    if(!this.isTripleLane){
        var path0 = new Path.Rectangle(new Point(curX,yStart), new Point(this.to.x, yEnd ));
        path0.fillColor = "white";
    }
    var path1 = new Path.Rectangle(new Point(this.from.x - Sidewalk.width,yStart), new Point(this.from.x, yEnd));
    path1.fillColor = Sidewalk.color;
    var path2 = new Path.Rectangle(new Point(this.to.x,yStart), new Point(this.to.x + Sidewalk.width,yEnd));
    path2.fillColor = Sidewalk.color;
};