
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

/*   if(color == "oldColor"){
          if(this.type == "houseplace") this.path.fillColor = "yellow";
          if(this.type == "workplace") this.path.fillColor = "purple";
          if(this.type == "funplace") this.path.fillColor = "blue";
    }
    else this.path.fillColor = color;*/

Zone.prototype.changeColor = function(color){
    lastPath.push(this.path);
};

Zone.prototype.draw = function(){
    "use strict";
    this.path = new Path.Rectangle(this.from,this.to);
    if(this.type == "houseplace") this.path.fillColor = "yellow";
    if(this.type == "workplace") this.path.fillColor = "purple";
    if(this.type == "funplace") this.path.fillColor = "blue";
    this.path.oldColor = this.path.fillColor;
    this.path.onMouseDown = this.myOnMouseDown;
    this.path.showing = false;
    this.path.myData = this.id;
};

Zone.prototype.showInfo = function(cars,pedestrians){
    this.path.tooltipLabel = new PointText(new Point(this.from.x + 12,this.to.y+3));
    this.path.tooltipLabel.fillColor = "#cc0000";
    this.path.tooltipLabel.textColor = "#cc0000";
    this.path.tooltipLabel.strokeColor = "#cc0000";
    this.path.tooltipLabel.strokeWidth = 0.6;
    this.path.tooltipLabel.fontSize = 8;
     //Name the tooltip so we can retrieve it later
    this.path.tooltipLabel.content = "Cars: " + parseInt(cars) + "\nPedestrians: "+ parseInt(pedestrians); //cercare questo campo//
    this.path.showing = true;
};

Zone.prototype.myOnMouseDown = function(){
    if(!this.showing){
        webSocket.send("ZONESTATE-"+this.myData)
        //webSocket.send("PATH-TRA0000004")
    }
    else{
        this.tooltipLabel.remove();
        this.showing = false;
    }
};