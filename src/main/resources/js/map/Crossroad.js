//Classe che rappresenta una corsia stradale
Crossroad.prototype.constructor = Crossroad;


//Costruttore
function Crossroad(id, from, category, jsonVertexes) {
    "use strict";
    this.id = id;
    this.from = from; //tipo point. punto in basso a sx
    this.to = new Point(this.from.x + Crossroad.width, this.from.y - Crossroad.width);
    this.category = category;
    this.path = null;
    this.vertexes = []; // di un vertice mi interessa, l'id del vicino, il tipo (1,2,3,4,5),
    //e anche il path del corrispondente simbolo direi,
    // così da poter cambiare il colore del semaforo per chi sta passando
    // le coordinate del punto
    this.buildVertexes(jsonVertexes);
    this.tramPath = null;
}

Crossroad.width = 24;
Crossroad.color = "#C0C1C4";
Crossroad.circleColor = "#616360";

Crossroad.prototype.changeColor = function(color){
    if(color == "oldColor") this.path.fillColor = Crossroad.color;
    else this.path.fillColor = color;
}

Crossroad.prototype.buildVertexes = function(jsonVertexes) {
    "use strict";
    //cerco
    for (var i = jsonVertexes.length - 1; i >= 0; i--) {
        var curVertex = jsonVertexes[i];
        var curVertexType = null;


        if ((this.from.x == (curVertex.point.x - Crossroad.width / 2)) && (this.from.y == curVertex.point.y))
            curVertexType = "down";
        if ((this.from.x == curVertex.point.x) && (this.from.y == (curVertex.point.y + Crossroad.width / 2)))
            curVertexType = "left";
        if ((this.from.x == (curVertex.point.x - Crossroad.width / 2)) && (this.from.y == (curVertex.point.y + Crossroad.width / 2)))
            curVertexType = "center";
        if ((this.from.x == (curVertex.point.x - Crossroad.width)) && (this.from.y == (curVertex.point.y + Crossroad.width / 2)))
            curVertexType = "right";
        if ((this.from.x == (curVertex.point.x - Crossroad.width / 2)) && (this.from.y == (curVertex.point.y + Crossroad.width)))
            curVertexType = "up";
        this.vertexes.push({
            id: curVertex.id,
            path: null,
            type: curVertexType,
            point: {
                x: curVertex.point.x,
                y: curVertex.point.y
            }
        });
    }
};

Crossroad.prototype.draw = function() {
    "use strict";
    this.path = new Path.Rectangle(this.from, this.to);
    this.path.fillColor = Crossroad.color;
    if (this.category == "roundabout") {
        var circlePath = new Path.Circle(new Point(this.from.x + Crossroad.width / 2, this.from.y - Crossroad.width / 2), Crossroad.width / 4);
        circlePath.fillColor = Crossroad.circleColor;
    }

    if (this.category == "classic") {
        this.drawClassic();
    }

    if (this.category == "semaphore") {
        this.drawSemaphore();
    }
    if (this.category == "angle") {
        this.drawAngle();
    }
};

Crossroad.prototype.drawClassic = function() {
    "use strict";
    var stopVertex = null;
    if ((this.vertexes[1].point.x == this.vertexes[0].point.x) || (this.vertexes[1].point.y == this.vertexes[0].point.y))
        stopVertex = this.vertexes[2];
    if ((this.vertexes[2].point.x == this.vertexes[0].point.x) || (this.vertexes[2].point.y == this.vertexes[0].point.y))
        stopVertex = this.vertexes[1];
    if ((this.vertexes[2].point.x == this.vertexes[1].point.x) || (this.vertexes[2].point.y == this.vertexes[1].point.y))
        stopVertex = this.vertexes[0];
    var fromPoint = null;
    var toPoint = null;
    if (stopVertex.type == "up") {
        fromPoint = new Point(stopVertex.point.x - Crossroad.width / 2, stopVertex.point.y + Sidewalk.width);
        toPoint = new Point(stopVertex.point.x + Crossroad.width / 2, stopVertex.point.y);
    }
    if (stopVertex.type == "left") {
        fromPoint = new Point(stopVertex.point.x, stopVertex.point.y + Crossroad.width / 2);
        toPoint = new Point(stopVertex.point.x + Sidewalk.width, stopVertex.point.y - Crossroad.width / 2);
    }
    if (stopVertex.type == "right") {
        fromPoint = new Point(stopVertex.point.x, stopVertex.point.y + Crossroad.width / 2);
        toPoint = new Point(stopVertex.point.x - Sidewalk.width, stopVertex.point.y - Crossroad.width / 2);
    }
    if (stopVertex.type == "down") {
        fromPoint = new Point(stopVertex.point.x - Crossroad.width / 2, stopVertex.point.y);
        toPoint = new Point(stopVertex.point.x + Crossroad.width / 2, stopVertex.point.y - Sidewalk.width);
    }
    var stopPath = new Path.Rectangle(fromPoint, toPoint);
    stopPath.fillColor = "#D90000";

};

Crossroad.prototype.drawSemaphore = function() {
    "use strict";
    for (var i = this.vertexes.length - 1; i >= 0; i--) {
        var curVertex = this.vertexes[i];
        var tram = false;
        var street = mapRegistry.getStreet(curVertex.id)
        if(street == null){
            console.log("Problem in getting the street")
        }

        if (curVertex.type == "up") {
            curVertex.path = new Path.Circle(new Point(curVertex.point.x, curVertex.point.y + Crossroad.width / 2 - Crossroad.width / 6), 2);
            curVertex.path.fillColor = "red";
            if(street.isTripleLane() && street.getTramPosition() == "left"){
                this.tramPath = new Path.Circle(new Point(curVertex.point.x - Crossroad.width /2 + Crossroad.width / 6, curVertex.point.y + Crossroad.width / 6),2)
                this.tramPath.fillColor = "red";
            }

        }
        if (curVertex.type == "down") {
            curVertex.path = new Path.Circle(new Point(curVertex.point.x, curVertex.point.y - Crossroad.width / 2 + Crossroad.width / 6), 2);
            curVertex.path.fillColor = "red";
            if(street.isTripleLane() && street.getTramPosition() == "right"){
                this.tramPath = new Path.Circle(new Point(curVertex.point.x + Crossroad.width /2 - Crossroad.width / 6, curVertex.point.y - Crossroad.width / 6),2)
                this.tramPath.fillColor = "red";
            }
        }
        if (curVertex.type == "right") {
            curVertex.path = new Path.Circle(new Point(curVertex.point.x - Crossroad.width / 2 + Crossroad.width / 6, curVertex.point.y), 2);
            curVertex.path.fillColor = "red";
            if(street.isTripleLane() && street.getTramPosition() == "up"){
                this.tramPath = new Path.Circle(new Point(curVertex.point.x - Crossroad.width / 6, curVertex.point.y - Crossroad.width / 2 + Crossroad.width/6),2)
                this.tramPath.fillColor = "red";
            }
        }
        if (curVertex.type == "left") {
            curVertex.path = new Path.Circle(new Point(curVertex.point.x + Crossroad.width / 2 - Crossroad.width / 6, curVertex.point.y), 2);
            curVertex.path.fillColor = "red";
            if(street.isTripleLane() && street.getTramPosition() == "down"){
                this.tramPath = new Path.Circle(new Point(curVertex.point.x + Crossroad.width / 6, curVertex.point.y + Crossroad.width / 2 - Crossroad.width/6),2)
                this.tramPath.fillColor = "red";
            }
        }
    }
};

Crossroad.prototype.drawAngle = function() {
    "use strict";


    for (var i = this.vertexes.length - 1; i >= 0; i--) {
        var curVertex = this.vertexes[i];

        if (curVertex.id != "nil") {
            var centerPoint = null;
            if (curVertex.type == "up") {
                centerPoint = new Point(curVertex.point.x + 1, curVertex.point.y + Crossroad.width / 2);
            }
            if (curVertex.type == "down") {
                centerPoint = new Point(curVertex.point.x + 1, curVertex.point.y - Crossroad.width / 2);
            }
            if (curVertex.type == "left") {
                centerPoint = new Point(curVertex.point.x + Crossroad.width / 2 +1, curVertex.point.y - 1);
            }
            if (curVertex.type == "right") {
                centerPoint = new Point(curVertex.point.x - Crossroad.width / 2, curVertex.point.y - 1);
            }
            var myPath = new Path.Rectangle(new Point(curVertex.point.x, curVertex.point.y), centerPoint);
            myPath.fillColor = "white";
        }
    }
};


Crossroad.prototype.changeLights = function(up,right,down,left,tram) {
    if(this.tramPath != null){
        this.tramPath.fillColor = tram;
    }
    for (var i = this.vertexes.length - 1; i >= 0; i--) {
        var curVertex = this.vertexes[i];
        if (curVertex.type == "up") {
            curVertex.path.fillColor = up;
        }
        if (curVertex.type == "down") {
            curVertex.path.fillColor = down;
        }
        if (curVertex.type == "right") {
            curVertex.path.fillColor = right;
        }
        if (curVertex.type == "left") {
            curVertex.path.fillColor = left;
        }
    }
};