//Classe che crea i vari elementi della mappa e li mantiene all'interno di apposite strutture dati
MapRegistry.prototype.constructor = Lane;

//costruttore
function MapRegistry() {
    "use strict";
    this.streets = {}; //oggetto usato come hashmap per avere tempo costante in accesso, uso l'id come chiave
   // this.lanes = {}; per ora non lo uso, passare attraverso le strade se si vuole la corsia
    this.crossroads = {};
    this.crosswalks = {};
    this.bus_stops = {};
    this.tram_stops = {}
    this.zones = {};
    this.streetsSize = 0;
    this.lanesSize = 0;
    this.dimensions = null;
}


//campo dati statico, in teoria non dovrebbe servire, ogni tanto mi serve per i test
MapRegistry.factor = 12;

//Funzione per invertire la cordinata y
MapRegistry.prototype.normalizeYCoordinate = function(yValue) {
    "use strict";
    return this.dimensions.y - yValue;
};

MapRegistry.prototype.getZone = function(id) {
    return this.zones[id];
};

MapRegistry.prototype.getCrossroad = function(id) {
    return this.crossroads[id];
};

MapRegistry.prototype.getCrosswalk = function(id) {
    return this.crosswalks[id];
};

MapRegistry.prototype.getStreet = function(id){
    return this.streets[id];
}

MapRegistry.prototype.getTramStop = function(id){
    return this.tram_stops[id];
}

MapRegistry.prototype.getBusStop = function(id){
    return this.bus_stops[id];
}


MapRegistry.prototype.getLane = function(id){
    var streetId = "R"+id.substring(1,id.length-1) + "0"
    var street = this.streets[streetId]
    var lane = null;
    for(var i = 0 ;i <  street.lanes.length;i++){
        if(street.lanes[i].id == id) lane = street.lanes[i]
    }
    if(lane == null) console.log("ERRORE: LANE NON TROVATA")
    return lane
}


//Funzione di avvio di costruzione della mappa.
//mapJson è l'oggetto creato a partire dal JSON che descrive la mappa
MapRegistry.prototype.buildMap = function(mapJson) {
    "use strict";
    var streets = mapJson.roads;
    var lanes = mapJson.lanes;
    var crossroads = mapJson.crossroads;
    var crosswalks = mapJson.pedestrian_crossroads;
    var zones = mapJson.zones;
    var bus_stops = mapJson.bus_stops;
    var tram_stops = mapJson.tram_stops;

    this.dimensions = {
        x: mapJson.dimensions.x,
        y: mapJson.dimensions.y
    };

    for (var i = streets.length - 1; i >= 0; i--) {
        var curStreet = streets[i];
        var lanesIDs = curStreet.lanesIDs;
        var numLanes = lanesIDs.length;
        var lanesFinded = 0;
        var curLanes = [];
        for (var j = lanes.length - 1; j >= 0 && lanesFinded != numLanes; j--) {
            var curLane = lanes[j];
            if (curLane.road == curStreet.id) {
                curLanes.push(curLane);
                lanesFinded += 1;
            }
        }
        curLanes.sort(function(a, b) {
            return a.id.localeCompare(b.id);
        });


        //moltiplico le coordinate per il fattore, sarà da eliminare
        /*
        curStreet.coordinates.begin.point.y;
        curStreet.coordinates.begin.point.x;
        curStreet.coordinates.end.point.y;
        curStreet.coordinates.end.point.x;
        */
        //inverto le coordinate y
        curStreet.coordinates.begin.point.y = this.normalizeYCoordinate(curStreet.coordinates.begin.point.y);
        curStreet.coordinates.end.point.y = this.normalizeYCoordinate(curStreet.coordinates.end.point.y);
        var myStreet = new Street(curStreet.id,
            new Point(curStreet.coordinates.begin.point.x, curStreet.coordinates.begin.point.y),
            new Point(curStreet.coordinates.end.point.x, curStreet.coordinates.end.point.y)
        );

        this.streets[myStreet.id] = myStreet;
        this.buildStreetLanes(myStreet, curLanes,false,"");
    }

    this.buildCrossroads(crossroads);
    this.buildCrosswalks(crosswalks);
    this.buildZones(zones);
    this.buildStops(bus_stops,"bus");
    this.buildStops(tram_stops,"tram")
};

MapRegistry.prototype.buildStops = function(stops,type){
    "use strict";
    for (var i = stops.length - 1; i >= 0; i--) {
        var curStop = stops[i];
        var curStopId = curStop.id;
        var copy = jQuery.extend(true, {}, this.streets[curStop.coordinates.begin.id])
        var isTripleLane = copy.isTripleLane();
        var isHorizontal = true;
        if(curStop.coordinates.begin.point.x == curStop.coordinates.end.point.x){
            isHorizontal = false;
        }
        curStop.coordinates.begin.point.y = this.normalizeYCoordinate(curStop.coordinates.begin.point.y);
        curStop.coordinates.end.point.y = this.normalizeYCoordinate(curStop.coordinates.end.point.y);
        copy.from = new Point(curStop.coordinates.begin.point.x, curStop.coordinates.begin.point.y);
        copy.to = new Point(curStop.coordinates.end.point.x, curStop.coordinates.end.point.y);
        var curLanes = copy.lanes;
        copy.lanes = [];
        copy.lines = [];
        copy.sidewalks = {};
        copy.id = curStopId;
        if(type== "bus"){
            this.buildStreetLanes(copy, curLanes,true,curStop.position);
            this.bus_stops[curStopId] = copy;
            }
        else{
            this.buildStreetLanes(copy, curLanes,true,"tram");
            this.tram_stops[curStopId] = copy;
        }
    }

}

MapRegistry.prototype.buildZones = function(zones){
    "use strict";

    for (var i = zones.length - 1; i >= 0; i--) {
        var curZone = zones[i];
        var curZoneId = curZone.id;
        var xZone = parseInt(curZoneId.substring(1, 8));
        var yZone = this.normalizeYCoordinate(parseInt(curZoneId.substring(8,15)));
        var offset = Lane.width + Sidewalk.width;
        var isTripleLane = this.streets[curZone.road].isTripleLane();
        if(isTripleLane) offset += Lane.width/2 + MyLine.width;
        var curPosition = curZone.position;
        if(curPosition == "left") xZone -= offset;
        if(curPosition == "right"){
         xZone += offset;
         if(!isTripleLane) xZone +=1;
     }
        if(curPosition == "up"){
            yZone -= offset;
            if(!isTripleLane)
                yZone -=1;
        }
        if(curPosition == "down") yZone += offset;
        var from = new Point(xZone,yZone);
        var myZone = new Zone(curZoneId,from,curZone.variety);
        this.zones[curZoneId] = myZone;
        myZone.draw();

    }
};


//Scorre il json delle strisce pedonali, le crea e le fa stampare sulla canvas
MapRegistry.prototype.buildCrosswalks = function(crosswalks){
    "use strict";
     for (var i = crosswalks.length - 1; i >= 0; i--) {
        var curCrosswalk = crosswalks[i];
        var isTripleLane = this.streets[curCrosswalk.coordinates.begin.id].isTripleLane();
        var xCross = curCrosswalk.coordinates.begin.point.x;
        var yCross = this.normalizeYCoordinate(curCrosswalk.coordinates.begin.point.y);
        var isHorizontal = true;
        if(curCrosswalk.coordinates.begin.point.x == curCrosswalk.coordinates.end.point.x){
            isHorizontal = false;
        }
        var bonus = 0;
        if(isTripleLane) bonus += Lane.width/2 + MyLine.width;
        if(isHorizontal) yCross += Crosswalk.width / 2 + bonus;
        else xCross -= Crosswalk.width / 2 + bonus;
        var fromPoint = new Point(xCross,yCross);
        var myCrosswalk = new Crosswalk(curCrosswalk.id,fromPoint,isHorizontal,isTripleLane);
        this.crosswalks[myCrosswalk.id] = myCrosswalk;
        myCrosswalk.draw();
     }
};


//funzione che costruisce gli incroci inserendoli nella hashmap e disegnandoli
MapRegistry.prototype.buildCrossroads = function(crossroads){
    "use strict";
    for (var i = crossroads.length - 1; i >= 0; i--) {
        var curCrossroad = crossroads[i];
        var crossroadId = curCrossroad.id;
        var xCross = parseInt(crossroadId.substring(1, 8));
        var yCross = this.normalizeYCoordinate(parseInt(crossroadId.substring(8, 15)));
        var vertexes = curCrossroad.vertexes;
        var mainVertex = null;
        var mainVertexIndex;
        var finded = false;
        for (var j = vertexes.length - 1; j >= 0 && (!finded); j--) {
            var curVertex = vertexes[j];
            if ((curVertex.point.x) == xCross && this.normalizeYCoordinate((curVertex.point.y)) == yCross) {
                mainVertex = curVertex;
                finded = true;
                mainVertexIndex = j;
            }
        }
        var mainVertexType = 3;
        if (mainVertex.id != "nil") {
            var xNear = parseInt(mainVertex.id.substring(1, 8));
            var yNear = this.normalizeYCoordinate(parseInt(mainVertex.id.substring(8, 15)));
            if (yNear != yCross) mainVertexType = 1;
            if (xNear != xCross) mainVertexType = 2;
        } else {
            var first = true;
            var nearVertex1 = null;
            var nearVertex2 = null;
            for (var j = vertexes.length - 1; j >= 0; j--) {
                if (j != mainVertexIndex) {
                    if (first) {
                        nearVertex1 = vertexes[j];
                        first = false;
                    } else nearVertex2 = vertexes[j];
                }
            }
            if (nearVertex1.point.x == nearVertex2.point.x) mainVertexType = 2;
            if (nearVertex1.point.y == nearVertex2.point.y) mainVertexType = 1;
        }
        var rectFrom = null;
        if (mainVertexType == 1) rectFrom = new Point(xCross - Crossroad.width / 2, yCross);
        if (mainVertexType == 2) rectFrom = new Point(xCross, yCross + Crossroad.width / 2);
        if (mainVertexType == 3) rectFrom = new Point(xCross - Crossroad.width / 2, yCross + Crossroad.width / 2);
        for (var j = vertexes.length - 1; j >= 0; j--) {

            vertexes[j].point.y = this.normalizeYCoordinate(vertexes[j].point.y);

        }
        var myCrossroad = new Crossroad(crossroadId,rectFrom, curCrossroad.category, vertexes);
        this.crossroads[myCrossroad.id] = myCrossroad;
        myCrossroad.draw();
    }

};

MapRegistry.prototype.buildStreetLanes = function(street, lanes, stop, position) {
    "use strict";
    var startPoint = street.from;
    var endPoint = street.to;
    var isHorizontal = false;
    if (startPoint.y == endPoint.y) isHorizontal = true;
    var isTripleLane = false;
    if (lanes.length == 3) isTripleLane = true;

    if (isHorizontal && isTripleLane) this.buildStreetHLanesT(street, lanes,stop,position);
    if (!isHorizontal && isTripleLane) this.buildStreetVLanesT(street, lanes,stop,position);
    if (isHorizontal && !isTripleLane) this.buildStreetHLanesD(street, lanes,stop,position);
    if (!isHorizontal && !isTripleLane) this.buildStreetVLanesD(street, lanes,stop,position);
};

//costruisce una strada orizzontale con tre corsie
MapRegistry.prototype.buildStreetHLanesT = function(street, lanes,stop,position) {
    "use strict";
    var startPoint = street.from;
    var endPoint = street.to;

    var tramIsUp = false;
    if(lanes[2].tram === true) tramIsUp = true;
    var stop1 = false;
    var stop2 = false;
    var stop3 = false;
    if(stop){
        if(position == "tram"){
            if(tramIsUp) stop3 = true;
            else stop1 = true;
        }
        if(position == "up" && tramIsUp) stop2 = true;
        if(position == "up" && !tramIsUp) stop3 = true;
        if(position == "down" && tramIsUp) stop1 = true;
        if(position == "down" && !tramIsUp) stop2 = true;
    }


    var yFrom0 = startPoint.y + (Lane.width + Lane.width / 2 + MyLine.width + Sidewalk.width);
    var yTo0 = startPoint.y + (Lane.width + Lane.width / 2 + MyLine.width);
    var from0 = new Point(startPoint.x, yFrom0);
    var to0 = new Point(endPoint.x, yTo0);
    var sidewalk0 = new Sidewalk(from0, to0);
    street.addSidewalk(sidewalk0, "down");
    sidewalk0.draw();


    var yFrom1 = startPoint.y + (Lane.width + Lane.width / 2 + MyLine.width);
    var yTo1 = startPoint.y + (Lane.width / 2 + MyLine.width);
    var from1 = new Point(startPoint.x, yFrom1);
    var to1 = new Point(endPoint.x, yTo1);
    var lane1 = new Lane(lanes[0].id, from1, to1, true, lanes[0].begintoend, lanes[0].tram,stop,stop1);
    street.addLane(lane1);
    lane1.draw();

    var yFrom2 = startPoint.y + (Lane.width / 2 + MyLine.width);
    var yTo2 = startPoint.y + (Lane.width / 2);
    var from2 = new Point(startPoint.x, yFrom2);
    var to2 = new Point(endPoint.x, yTo2);
    var line2 = new MyLine(from2, to2);
    street.addLine(line2);
    line2.draw();

    var yFrom3 = startPoint.y + (Lane.width / 2);
    var yTo3 = startPoint.y - (Lane.width / 2);
    var from3 = new Point(startPoint.x, yFrom3);
    var to3 = new Point(endPoint.x, yTo3);
    var lane3 = new Lane(lanes[1].id, from3, to3, true, lanes[1].begintoend, lanes[1].tram,stop,stop2);
    street.addLane(lane3);
    lane3.draw();

    var yFrom4 = startPoint.y - (Lane.width / 2);
    var yTo4 = startPoint.y - (Lane.width / 2 + MyLine.width);
    var from4 = new Point(startPoint.x, yFrom4);
    var to4 = new Point(endPoint.x, yTo4);
    var line4 = new MyLine(from4, to4);
    street.addLine(line4);
    line4.draw();

    var yFrom5 = startPoint.y - (Lane.width / 2 + MyLine.width);
    var yTo5 = startPoint.y - (Lane.width + Lane.width / 2 + MyLine.width);
    var from5 = new Point(startPoint.x, yFrom5);
    var to5 = new Point(endPoint.x, yTo5);
    var lane5 = new Lane(lanes[2].id, from5, to5, true, lanes[2].begintoend, lanes[2].tram,stop,stop3);
    street.addLane(lane5);
    lane5.draw();

    var yFrom6 = startPoint.y - (Lane.width + Lane.width / 2 + MyLine.width);
    var yTo6 = startPoint.y - (Lane.width + Lane.width / 2 + MyLine.width + Sidewalk.width);
    var from6 = new Point(startPoint.x, yFrom6);
    var to6 = new Point(endPoint.x, yTo6);
    var sidewalk6 = new Sidewalk(from6, to6);
    street.addSidewalk(sidewalk6, "up");
    sidewalk6.draw();

};

MapRegistry.prototype.buildStreetVLanesT = function(street, lanes,stop,position) {
    "use strict";
    var startPoint = street.from;
    var endPoint = street.to;

    var tramIsRight = false;
    if(lanes[2].tram === true) tramIsRight = true;
    var stop1 = false;
    var stop2 = false;
    var stop3 = false;
    if(stop){
        if(position == "tram"){
            if(tramIsRight) stop3 = true;
            else stop1 = true;
        }
        if(position == "right" && tramIsRight) stop2 = true;
        if(position == "right" && !tramIsRight) stop3 = true;
        if(position == "left" && tramIsRight) stop1 = true;
        if(position == "left" && !tramIsRight) stop2 = true;
    }


    var xFrom0 = startPoint.x - (Lane.width + Lane.width / 2 + MyLine.width + Sidewalk.width);
    var xTo0 = startPoint.x - (Lane.width + Lane.width / 2 + MyLine.width);
    var from0 = new Point(xFrom0, startPoint.y);
    var to0 = new Point(xTo0, endPoint.y);
    var sidewalk0 = new Sidewalk(from0, to0);
    street.addSidewalk(sidewalk0, "left");
    sidewalk0.draw();

    var xFrom1 = startPoint.x - (Lane.width + Lane.width / 2 + MyLine.width);
    var xTo1 = startPoint.x - (Lane.width / 2 + MyLine.width);
    var from1 = new Point(xFrom1, startPoint.y);
    var to1 = new Point(xTo1, endPoint.y);
    var lane1 = new Lane(lanes[0].id, from1, to1, false, lanes[0].begintoend, lanes[0].tram,stop,stop1);
    street.addLane(lane1);
    lane1.draw();

    var xFrom2 = startPoint.x - (Lane.width / 2 + MyLine.width);
    var xTo2 = startPoint.x - (Lane.width / 2);
    var from2 = new Point(xFrom2, startPoint.y);
    var to2 = new Point(xTo2, endPoint.y);
    var line2 = new MyLine(from2, to2);
    street.addLine(line2);
    line2.draw();

    var xFrom3 = startPoint.x - (Lane.width / 2);
    var xTo3 = startPoint.x + (Lane.width / 2);
    var from3 = new Point(xFrom3, startPoint.y);
    var to3 = new Point(xTo3, endPoint.y);
    var lane3 = new Lane(lanes[1].id, from3, to3, false, lanes[1].begintoend, lanes[1].tram,stop,stop2);
    street.addLane(lane3);
    lane3.draw();

    var xFrom4 = startPoint.x + (Lane.width / 2);
    var xTo4 = startPoint.x + (Lane.width / 2 + MyLine.width);
    var from4 = new Point(xFrom4, startPoint.y);
    var to4 = new Point(xTo4, endPoint.y);
    var line4 = new MyLine(from4, to4);
    street.addLine(line4);
    line4.draw();

    var xFrom5 = startPoint.x + (Lane.width / 2 + MyLine.width);
    var xTo5 = startPoint.x + (Lane.width + Lane.width / 2 + MyLine.width);
    var from5 = new Point(xFrom5, startPoint.y);
    var to5 = new Point(xTo5, endPoint.y);
    var lane5 = new Lane(lanes[2].id, from5, to5, false, lanes[2].begintoend, lanes[2].tram,stop,stop3);
    street.addLane(lane5);
    lane5.draw();

    var xFrom6 = startPoint.x + (Lane.width + Lane.width / 2 + MyLine.width);
    var xTo6 = startPoint.x + (Lane.width + Lane.width / 2 + MyLine.width + Sidewalk.width);
    var from6 = new Point(xFrom6, startPoint.y);
    var to6 = new Point(xTo6, endPoint.y);
    var sidewalk6 = new Sidewalk(from6, to6);
    street.addSidewalk(sidewalk6, "right");
    sidewalk6.draw();
};

MapRegistry.prototype.buildStreetHLanesD = function(street, lanes,stop,position) {
    "use strict";
    var startPoint = street.from;
    var endPoint = street.to;

    var stop1 = false;
    var stop2 = false;
    if(stop){
        if(position == "up") stop2 = true;
        if(position == "down") stop1 = true;
    }


    var yFrom0 = startPoint.y + (Lane.width + Sidewalk.width);
    var yTo0 = startPoint.y + (Lane.width);
    var from0 = new Point(startPoint.x, yFrom0);
    var to0 = new Point(endPoint.x, yTo0);
    var sidewalk0 = new Sidewalk(from0, to0);
    street.addSidewalk(sidewalk0, "down");
    sidewalk0.draw();

    var yFrom1 = startPoint.y + (Lane.width);
    var yTo1 = startPoint.y;
    var from1 = new Point(startPoint.x, yFrom1);
    var to1 = new Point(endPoint.x, yTo1);
    var lane1 = new Lane(lanes[0].id, from1, to1, true, lanes[0].begintoend, lanes[0].tram,stop,stop1);
    street.addLane(lane1);
    lane1.draw();

    var yFrom2 = startPoint.y;
    var yTo2 = startPoint.y - (MyLine.width);
    var from2 = new Point(startPoint.x, yFrom2);
    var to2 = new Point(endPoint.x, yTo2);
    var line2 = new MyLine(from2, to2);
    street.addLine(line2);
    line2.draw();

    var yFrom3 = startPoint.y - (MyLine.width);
    var yTo3 = startPoint.y - (MyLine.width + Lane.width);
    var from3 = new Point(startPoint.x, yFrom3);
    var to3 = new Point(endPoint.x, yTo3);
    var lane3 = new Lane(lanes[1].id, from3, to3, true, lanes[1].begintoend, lanes[1].tram,stop,stop2);
    street.addLane(lane3);
    lane3.draw();

    var yFrom4 = startPoint.y - (MyLine.width + Lane.width);
    var yTo4 = startPoint.y - (MyLine.width + Lane.width +  Sidewalk.width);
    var from4 = new Point(startPoint.x, yFrom4);
    var to4 = new Point(endPoint.x, yTo4);
    var sidewalk4 = new Sidewalk(from4, to4);
    street.addSidewalk(sidewalk4, "up");
    sidewalk4.draw();


};

MapRegistry.prototype.buildStreetVLanesD = function(street, lanes,stop,position) {
    "use strict";
    var startPoint = street.from;
    var endPoint = street.to;
    var stop1 = false;
    var stop2 = false;
    if(stop){
        if(position == "right") stop2 = true;
        if(position == "left") stop1 = true;
    }

    var xFrom0 = startPoint.x - (Lane.width + Sidewalk.width);
    var xTo0 = startPoint.x - (Lane.width);
    var from0 = new Point(xFrom0, startPoint.y);
    var to0 = new Point(xTo0, endPoint.y);
    var sidewalk0 = new Sidewalk(from0, to0);
    street.addSidewalk(sidewalk0, "left");
    sidewalk0.draw();

    var xFrom1 = startPoint.x - (Lane.width);
    var xTo1 = startPoint.x;
    var from1 = new Point(xFrom1, startPoint.y);
    var to1 = new Point(xTo1, endPoint.y);
    var lane1 = new Lane(lanes[0].id, from1, to1, false, lanes[0].begintoend, lanes[0].tram,stop,stop1);
    street.addLane(lane1);
    lane1.draw();

    var xFrom2 = startPoint.x;
    var xTo2 = startPoint.x + (MyLine.width);
    var from2 = new Point(xFrom2, startPoint.y);
    var to2 = new Point(xTo2, endPoint.y);
    var line2 = new MyLine(from2, to2);
    street.addLine(line2);
    line2.draw();

    var xFrom3 = startPoint.x + (MyLine.width);
    var xTo3 = startPoint.x + (MyLine.width + Lane.width);
    var from3 = new Point(xFrom3, startPoint.y);
    var to3 = new Point(xTo3, endPoint.y);
    var lane3 = new Lane(lanes[1].id, from3, to3, false, lanes[1].begintoend, lanes[1].tram,stop,stop2);
    street.addLane(lane3);
    lane3.draw();


    var xFrom4 = startPoint.x + (MyLine.width + Lane.width);
    var xTo4 = startPoint.x + (MyLine.width + Lane.width + Sidewalk.width);
    var from4 = new Point(xFrom4, startPoint.y);
    var to4 = new Point(xTo4, endPoint.y);
    var sidewalk4 = new Sidewalk(from4, to4);
    street.addSidewalk(sidewalk4, "right");
    sidewalk4.draw();

};


/*
//funzione per quando il mouse passa sopra una entità
function myOnMouseEnter(event) {
    "use strict";
    //var tooltipRect = new Rectangle(this.position + new Point(40, 40), new Size(100, 100));
    this.tooltipLabel = new PointText(this.position.subtract(new Point(-5, -5)));
    this.tooltipLabel.fillColor = "white";
    this.tooltipLabel.textColor = "blue";
    this.tooltipLabel.strokeColor = "black";
    // Name the tooltip so we can retrieve it later
    this.tooltipLabel.content = this.myData.id; //cercare questo campo//
    this.tooltipLabel.bringToFront();
};

function myOnMouseLeave(event) {
    "use strict";
    // We retrieve the tooltip from its name in the parent node (group) then remove it
    this.tooltipLabel.remove();
};*/