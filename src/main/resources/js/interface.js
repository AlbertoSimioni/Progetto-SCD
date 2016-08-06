paper.install(window);
//sposta il centro della canvas visualizzato
function traslladar(a, b) {
        var center = paper.project.view.center;
        var desX = (a.x - b.x);
        var desY = (a.y - b.y);

        var newCenter = [ center.x + desX, center.y + desY ];
        return newCenter;
    }

    function zoomIn() {
        view.zoom = view.zoom * 1.1;
    }

    function zoomOut() {
        view.zoom = view.zoom / 1.1;
    }

var webSocket = null;
var lastPath = [];
var lastEntityID = null;
var mapRegistry = new MapRegistry();
var registry = new EntitiesRegistry();
var pathDrawing = false;

window.onload = function() {
    // Get a reference to the canvas object
    var canvas = document.getElementById('myCanvas');
    // Create an empty project and a view for the canvas:
    paper.setup(canvas);
    var amount = 2;
    $(function () {
                $('[data-toggle="tooltip"]').tooltip()
            })


            var myTool = new paper.Tool();
            myTool.onMouseDown = function(event) {
                path = new Point();
                path.add(event.point);
                $("#myCanvas").css('cursor', '-webkit-grabbing');

            };

            myTool.onMouseDrag = function(event) {
                path.add(event.point);

                var des = traslladar(event.downPoint, event.point);
                paper.project.view.center = des;

            }

            myTool.onMouseUp = function(event) {
                $("#myCanvas").css('cursor', '-webkit-grab');
                $("#myCanvas").css('cursor', '-moz-grab');
            }

            $('#myCanvas').mousewheel(function(event) {
                event.deltaY > 0 ? zoomIn() : zoomOut();
            });
    var amount = 2;
    paper.view.setViewSize(1080, 800)
    paper.view.draw();
    paper.view.autoUpdate = false;
    paper.view.onFrame = function(event) {
       // Every frame, rotate the path by 3 degrees:
       if(!pathDrawing)
        paper.view.update()
   }



    var WS = window['MozWebSocket'] ? MozWebSocket : WebSocket
    webSocket = new WS("ws" + location.protocol.substring(4) + "//" + window.location.hostname + ":6696/ws")
    $(".alert").alert()
    webSocket.onopen = function() {
       // Web Socket is connected, send data using send()
        webSocket.send("MAP-1");
        };
    webSocket.onmessage = function(event) {
        var msg = JSON.parse(event.data);
        console.log(JSON.stringify(msg))
        if(msg.hasOwnProperty('dimensions')){ //MAP ARRIVED
            mapRegistry.buildMap(msg);
            paper.view.draw();
            fitMap();
        }
        if (msg.type == "CarPosition") {
            var lat = msg.info.lat
            var long = mapRegistry.normalizeYCoordinate(msg.info.long)
            var car = registry.findCar(msg.info.id)
            if (car == null) {

                registry.addCar(msg.info.id,lat,long,msg.info.direction)
            } else {
                car.move(lat,long, msg.info.direction)
                car.show();
            }
        }
        if (msg.type == "PedestrianPosition") {
            var lat = msg.info.lat
            var long = mapRegistry.normalizeYCoordinate(msg.info.long)
            var pedestrian = registry.findPedestrian(msg.info.id)
            if (pedestrian == null) {
                registry.addPedestrian(msg.info.id,lat,long,msg.info.direction)
            } else {
                pedestrian.move(lat,long, msg.info.direction)
                pedestrian.show();
            }
        }
        if (msg.type == "TramPosition") {
            var lat = msg.info.lat
            var long = mapRegistry.normalizeYCoordinate(msg.info.long)
            var tram = registry.findTram(msg.info.id)
            if (tram == null) {
                registry.addTram(msg.info.id,lat,long,msg.info.direction)
            } else {
                tram.move(lat,long, msg.info.direction)
                tram.show();
            }
        }
        if (msg.type == "BusPosition") {
            var lat = msg.info.lat
            var long = mapRegistry.normalizeYCoordinate(msg.info.long)
            var bus = registry.findBus(msg.info.id)
            if (bus == null) {
                registry.addBus(msg.info.id,lat,long,msg.info.direction)
            } else {
                bus.move(lat,long, msg.info.direction)
                bus.show();
            }
        }

        if(msg.type == "HideCar"){
            var car = registry.findCar(msg.info.id);
            if(car != null){
                car.hide();
            }
        }
        if(msg.type == "HideBus"){
            var bus = registry.findBus(msg.info.id);
            if(bus != null){
                bus.hide();
            }
        }
        if(msg.type == "HideTram"){
            var tram = registry.findTram(msg.info.id);
            if(tram != null){
                tram.hide();
            }
        }
        if(msg.type == "HidePedestrian"){
            var pedestrian = registry.findPedestrian(msg.info.id);
            if(pedestrian != null){
                pedestrian.hide();
            }
        }
        if(msg.type == "SemaphoreState"){
            var semaphore = mapRegistry.crossroads[msg.info.id];
            if (semaphore.category == "semaphore"){
                semaphore.changeLights(msg.info.up,msg.info.right,msg.info.down,msg.info.left,msg.info.tram)
            }
            else{
                console.log("We shouldn't be here")
            }
        }
        if(msg.type == "time"){
            var minutes = msg.info.minutes;
            var hours = msg.info.hours;
            minutes = checkTime(minutes);
            hours = checkTime(hours);
            var text = hours + ":" +  minutes;
            $( "#timeText" ).text(text);
        }

        if(msg.type == "path"){
            var  path = msg.steps;
            pathDrawing = true;
            //colorSteps(lastPath,"oldColor");
            changeColorPath("oldColor");
            lastPath = [];
            colorSteps(path,"#00ccff");
            changeColorPath("#00ccff");
            //lastPath = path;
            pathDrawing = false;
        }

        if(msg.type == "zoneState"){
            var zoneID = msg.info.id;
            var pedestrians = msg.info.pedestrians;
            var cars = msg.info.cars;
            var zone = mapRegistry.getZone(zoneID);
            zone.showInfo(cars,pedestrians);
        }

    }

    // if errors on websocket
    var onalert = function(event) {
        $(".alert").removeClass("hide")
    }

    webSocket.onerror = onalert
    webSocket.onclose = onalert
}

function checkTime(i) {
    if (i < 10) {i = "0" + i};  // add zero in front of numbers < 10
    return i;
}

function changeColorPath(color){
    if(color == "oldColor"){
        for(var i in lastPath){
            lastPath[i].fillColor = lastPath[i].oldColor;

        }
    }
    else{
        for(var i in lastPath){
            lastPath[i].fillColor = color;

        }
    }
}

function colorSteps(steps,color){
   for(var i in steps)
   {
     var step = steps[i];
     var id = step.id;
     var type = step.type;
     if(type == "lane"){
        var lane = mapRegistry.getLane(id);
        //change
        lane.changeColor(color);
     }
     if(type == "road"){
        var position = step.position;
        mapRegistry.getStreet(id).changeColor(color,position);

     }
     else if(type == "crossroad"){
        mapRegistry.getCrossroad(id).changeColor(color);
     }

     else if(type == "zone"){
        mapRegistry.getZone(id).changeColor(color);
     }

     else if(type == "bus_stop"){
        var busStop = mapRegistry.getBusStop(id);
        var position = step.position;
        var entityType = step.entity;
        busStop.changeColorBusStop(color,position,entityType)
     }

     else if(type == "tram_stop"){
        var tramStop = mapRegistry.getTramStop(id);
        var position = step.position;
        var entityType = step.entity;
        tramStop.changeColorTramStop(color,position,entityType)
     }

     else if(type == "crosswalk"){
        var entityType = step.entity;
        var position = step.position;
        var positionPrec = steps[i-1].position;
        if(entityType == "P"){
            var crosswalk = mapRegistry.getCrosswalk(id);
            if(position === positionPrec){
                crosswalk.changeColorSidewalk(color,positionPrec);
            }
            else{
                crosswalk.changeColor(color);
            }
        }
     }


   }
}