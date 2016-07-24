Street.prototype.constructor = Street;

function Street(id, from, to) {
    "use strict";
    this.id = id;
    this.from = from;
    this.to = to;
    this.lanes = [];
    this.lines = [];
    this.sidewalks = {};
};


Street.prototype.addLane = function(lane){
    "use strict";
    this.lanes.push(lane);
};

Street.prototype.addLine = function(line){
    "use strict";
    this.lines.push(line);
};

//position può essere "up/down/left/right"
Street.prototype.addSidewalk = function(sidewalk,position){
    "use strict";
    this.sidewalks[position] = sidewalk;
}

Street.prototype.isTripleLane = function(){
    if(this.lanes.length == 3) return true;
    else return false;
}

Street.prototype.getTramPosition = function(){
    "use strict"
    if(this.lanes.length == 2) return "";
    if(this.lanes[0].isHorizontal){
        if(this.lanes[0].tram) return "down";
        else return "up";
    }
    else{
        if(this.lanes[0].tram) return "left";
        else return "right";
    }
}