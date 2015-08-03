function EntitiesRegistry() {
    "use strict";
    this.cars = [];
    this.pedestrians = [];
    this.style = new EntitiesStyle();
}

EntitiesRegistry.prototype.addCar = function(id, position) {
    "use strict";
    var curCar = new Car(id, position);
    curCar.draw();
    curCar.show();
    this.cars[id] = curCar;
    return curCar;
}


EntitiesRegistry.prototype.show = function() {
    "use strict";
    for (var i in this.cars) {
        this.cars[i].show();
    }
}

EntitiesRegistry.prototype.draw = function() {
    "use strict";
    for (var i in this.cars) {
        this.cars[i].draw();
    }
}

EntitiesRegistry.prototype.hide = function() {
    "use strict";
    for (var i in this.cars) {
        this.cars[i].hide();
    }
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

EntitiesRegistry.prototype.addPedestrian = function(id, position)
{
    var curPed = new Pedestrian(id, position);
    cur.draw(this.style.pedestrian);
    cur.show();
    this.pedestrians[id_quartiere_abitante+"_"+id] = cur;
    if(typeof this.onCarsChange === 'function')
    {
        this.onPedestriansChange(this.pedestrians);
    }
    return cur;
}

EntitiesRegistry.prototype.removePedestrian = function(id)
{
    var toDel = this.pedestrians[id_quartiere_abitante+"_"+id];
    if(toDel)
    {
        toDel.remove();
        delete this.pedestrians[id_quartiere_abitante+"_"+id];
        if(typeof this.onCarsChange === 'function')
        {
            this.onPedestriansChange(this.pedestrians);
        }
    }
}