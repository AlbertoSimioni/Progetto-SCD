function EntitiesRegistry() {
    "use strict";
    this.cars = [];
    this.pedestrians = [];
    this.trams = [];
    this.buses = [];
}



/*
EntitiesRegistry.prototype.show = function() {
    "use strict";
    for (var i in this.cars) {
        this.cars[i].show();
    }
}*/
/*
EntitiesRegistry.prototype.draw = function() {
    "use strict";
    for (var i in this.cars) {
        this.cars[i].draw();
    }
}*/
/*
EntitiesRegistry.prototype.hide = function() {
    "use strict";
    for (var i in this.cars) {
        this.cars[i].hide();
    }
}*/

EntitiesRegistry.prototype.addCar = function(id, lat,long, direction) {
    "use strict";
    var curCar = new Car(id, lat,long, direction);
    //curCar.draw();
    curCar.show();
    this.cars[id] = curCar;
    return curCar;
}

EntitiesRegistry.prototype.findCar = function(id) {
    var car = null
    for (var i in this.cars) {
        if (i == id) {
            car = this.cars[i];
            break;
        }
    }
    return car
}

EntitiesRegistry.prototype.addPedestrian = function(id, lat,long, direction)
{
    "use strict";
    var curPedestrian = new Pedestrian(id, lat,long, direction);
    //curCar.draw();
    curPedestrian.show();
    this.pedestrians[id] = curPedestrian;
    return curPedestrian;
}

EntitiesRegistry.prototype.findPedestrian = function(id) {
    var pedestrian = null
    for (var i in this.pedestrians) {
        if (i == id) {
            pedestrian = this.pedestrians[i];
            break;
        }
    }
    return pedestrian;
}


EntitiesRegistry.prototype.addBus = function(id, lat,long, direction)
{
    "use strict";
    var curBus = new Bus(id, lat,long, direction);
    //curCar.draw();
    curBus.show();
    this.buses[id] = curBus;
    return curBus;
}

EntitiesRegistry.prototype.findBus = function(id) {
    var bus = null
    for (var i in this.buses) {
        if (i == id) {
            bus = this.buses[i];
            break;
        }
    }
    return bus;
}

EntitiesRegistry.prototype.addTram = function(id, lat,long, direction)
{
    "use strict";
    var curTram = new Tram(id, lat,long, direction);
    //curCar.draw();
    curTram.show();
    this.trams[id] = curTram;
    return curTram;
}

EntitiesRegistry.prototype.findTram = function(id) {
    var tram = null
    for (var i in this.trams) {
        if (i == id) {
            tram = this.trams[i];
            break;
        }
    }
    return tram;
}
