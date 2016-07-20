Street.prototype.constructor = BusStop;

function BusStop(id, from, to) {
    "use strict";
    this.id = id;
    this.from = from;
    this.to = to;
};
