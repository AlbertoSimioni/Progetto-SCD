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
    // Create a new path and style it:
    var path = new paper.Path({
        // 80% black:
        strokeColor: [0.8],
        strokeWidth: 30,
        strokeCap: 'square'
    });

    // Add 5 segment points to the path spread out
    // over the width of the view:
    for (var i = 0; i <= amount; i++) {
        path.add(new Point((i / amount) * view.size.width, view.center.y));
    }
    paper.view.setViewSize(1100, 800)
    paper.view.draw();


    var registry = new EntitiesRegistry()



    var WS = window['MozWebSocket'] ? MozWebSocket : WebSocket
    var mapSocket = new WS("ws" + location.protocol.substring(4) + "//" + window.location.hostname + ":6696/ws")
    $(".alert").alert()
    mapSocket.onmessage = function(event) {
        var msg = JSON.parse(event.data);
        /* if(msg.type == "NewCar"){
             console.log("4444444")
               registry.addCar(msg.id, msg.position,9)
          }*/
        //msg.info.position = msg.info.position * 50;
        var position = msg.info.position
        console.log(JSON.stringify(msg))
        if (msg.type == "CarMoved") {
            var car = registry.findCar(msg.info.id)
            console.log(JSON.stringify(car))
            if (!car) {
                registry.addCar(msg.info.id,position)
            } else {
                car.move(position, 90)
            }
        }
        /*
            var rasterCar = new paper.Raster('car');

            // Move the raster to the center of the view
            rasterCar.position.x = 0;
            rasterCar.position.y = view.center.y;
           	// Draw the view now:
            paper.view.draw();
            view.onFrame = function(event) {
               // On each frame, rotate the path by 3 degrees:
               rasterCar.position.x += 1;
            }*/
    }

    // if errors on websocket
    var onalert = function(event) {
        $(".alert").removeClass("hide")
    }

    mapSocket.onerror = onalert
    mapSocket.onclose = onalert
}