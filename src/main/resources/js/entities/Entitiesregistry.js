function EntitiesRegistry() {
    "use strict";
    this.cars = {};
    this.pedestrians = {};
    this.trams = {};
    this.buses = {};
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
    curCar.draw();
    curCar.show();
    this.cars[id] = curCar;
    return curCar;
}

EntitiesRegistry.prototype.findCar = function(id) {
  return this.cars[id];
}

EntitiesRegistry.prototype.addPedestrian = function(id, lat,long, direction)
{
    "use strict";
    var curPedestrian = new Pedestrian(id, lat,long, direction);
    curPedestrian.draw();
    curPedestrian.show();
    this.pedestrians[id] = curPedestrian;
    return curPedestrian;
}

EntitiesRegistry.prototype.findPedestrian = function(id) {
  return this.pedestrians[id];
}


EntitiesRegistry.prototype.addBus = function(id, lat,long, direction)
{
    "use strict";
    var curBus = new Bus(id, lat,long, direction);
    curBus.draw();
    curBus.show();
    this.buses[id] = curBus;
    return curBus;
}

EntitiesRegistry.prototype.findBus = function(id) {
  return this.buses[id];
}

EntitiesRegistry.prototype.addTram = function(id, lat,long, direction)
{
    "use strict";
    var curTram = new Tram(id, lat,long, direction);
    curTram.draw();
    curTram.show();
    this.trams[id] = curTram;
    return curTram;
}

EntitiesRegistry.prototype.findTram = function(id) {
  return this.trams[id];
}
