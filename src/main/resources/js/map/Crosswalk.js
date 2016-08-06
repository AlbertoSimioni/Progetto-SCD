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
    this.whitePaths = [];
    this.sidewalkPaths = [];
    this.isTripleLane = isTripleLane;
}

Crosswalk.width = 12;
Crosswalk.linesWidth = 2;


Crosswalk.prototype.changeColorSidewalk = function(color,position){
    if(position == "down"){
        sidewalk = this.sidewalkPaths[0];
    }
    else if(position == "up"){
        sidewalk = this.sidewalkPaths[1];
    }
    else if(position == "left"){
        sidewalk = this.sidewalkPaths[0];
    }
    else if(position == "right"){
        sidewalk = this.sidewalkPaths[1];
    }
    /*
    if(color == "oldColor"){
        sidewalk.fillColor = Sidewalk.color;
    }
    else{
        sidewalk.fillColor = color;
    }*/
    lastPath.push(sidewalk);

}
Crosswalk.prototype.changeColor = function(color){
     for(var i = 0 ;i <  this.whitePaths.length;i++){
        /*if(color == "oldColor")
            this.whitePaths[i].fillColor = "white"
        else
            this.whitePaths[i].fillColor = color*/
         lastPath.push(this.whitePaths[i]);
     }
}

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
            path.oldColor = "white"
            this.whitePaths.push(path);
        }
        else { path.fillColor = "black"; path.oldColor = "black"; }
        white = !white;
        curY -= Crosswalk.linesWidth;
    }
    if(!this.isTripleLane){
        var path0 = new Path.Rectangle(new Point(xStart,curY), new Point(xEnd, this.to.y));
        path0.fillColor = "white";
        path0.oldColor = "white";
        this.whitePaths.push(path0);
    }
    var path1 = new Path.Rectangle(new Point(xStart,this.from.y + Sidewalk.width), new Point(xEnd,this.from.y));
    path1.oldColor = Sidewalk.color;
    path1.fillColor = Sidewalk.color;
    this.sidewalkPaths.push(path1);
    var path2 = new Path.Rectangle(new Point(xStart,this.to.y), new Point(xEnd,this.to.y - Sidewalk.width));
    path2.oldColor = Sidewalk.color;
    path2.fillColor = Sidewalk.color;
    this.sidewalkPaths.push(path2);
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
            path.oldColor = "white";
            this.whitePaths.push(path);
        }
        else{ path.fillColor = "black"; path.oldColor = "black"; }
        white = !white;
        curX += Crosswalk.linesWidth;
    }
    if(!this.isTripleLane){
        var path0 = new Path.Rectangle(new Point(curX,yStart), new Point(this.to.x, yEnd ));
        path0.fillColor = "white";
        path0.oldColor = "white";
        this.whitePaths.push(path0);
    }
    //sidewalks
    var path1 = new Path.Rectangle(new Point(this.from.x - Sidewalk.width,yStart), new Point(this.from.x, yEnd));
    path1.fillColor = Sidewalk.color;
    path1.oldColor = Sidewalk.color;
    this.sidewalkPaths.push(path1);
    var path2 = new Path.Rectangle(new Point(this.to.x,yStart), new Point(this.to.x + Sidewalk.width,yEnd));
    path2.fillColor = Sidewalk.color;
    path2.oldColor = Sidewalk.color;
    this.sidewalkPaths.push(path2);
};