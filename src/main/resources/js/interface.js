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
    paper.view.setViewSize(1100, 800)
    paper.view.draw();


    var registry = new EntitiesRegistry()
    var mapRegistry = new MapRegistry();


    var WS = window['MozWebSocket'] ? MozWebSocket : WebSocket
    var mapSocket = new WS("ws" + location.protocol.substring(4) + "//" + window.location.hostname + ":6696/ws")
    $(".alert").alert()
    mapSocket.onopen = function() {
       // Web Socket is connected, send data using send()
        mapSocket.send("MAP");
        };
    mapSocket.onmessage = function(event) {
        var msg = JSON.parse(event.data);
        //console.log(JSON.stringify(msg))
        if(msg.hasOwnProperty('dimensions')){ //MAP ARRIVED
            mapRegistry.buildMap(msg);
            paper.view.draw();
        }
        if (msg.type == "CarPosition") {
            var lat = msg.info.lat
            var long = msg.info.long
            var car = registry.findCar(msg.info.id)
            if (!car) {
                registry.addCar(msg.info.id,lat,long,msg.info.direction)
            } else {
                car.move(lat,long, msg.info.direction)
                car.show();
            }
        }
        if (msg.type == "PedestrianPosition") {
            var lat = msg.info.lat
            var long = msg.info.long
            var pedestrian = registry.findPedestrian(msg.info.id)
            if (!pedestrian) {
                registry.addPedestrian(msg.info.id,lat,long,msg.info.direction)
            } else {
                pedestrian.move(lat,long, msg.info.direction)
                pedestrian.show();
            }
        }
        if (msg.type == "TramPosition") {
            var lat = msg.info.lat
            var long = msg.info.long
            var tram = registry.findTram(msg.info.id)
            if (!tram) {
                registry.addTram(msg.info.id,lat,long,msg.info.direction)
            } else {
                tram.move(lat,long, msg.info.direction)
                tram.show();
            }
        }
        if (msg.type == "BusPosition") {
            var lat = msg.info.lat
            var long = msg.info.long
            var bus = registry.findBus(msg.info.id)
            if (!bus) {
                registry.addBus(msg.info.id,lat,long,msg.info.direction)
            } else {
                bus.move(lat,long, msg.info.direction)
                bus.show();
            }
        }

        if(msg.type == "HideCar"){
            var car = registry.findCar(msg.info.id);
            if(car){
                car.hide();
            }
        }
        if(msg.type == "HideBus"){
            var bus = registry.findBus(msg.info.id);
            if(bus){
                bus.hide();
            }
        }
        if(msg.type == "HideTram"){
            var tram = registry.findTram(msg.info.id);
            if(tram){
                tram.hide();
            }
        }
        if(msg.type == "HidePedestrian"){
            var pedestrian = registry.findPedestrian(msg.info.id);
            if(pedestrian){
                pedestrian.hide();
            }
        }


    }

    // if errors on websocket
    var onalert = function(event) {
        $(".alert").removeClass("hide")
    }

    mapSocket.onerror = onalert
    mapSocket.onclose = onalert
}