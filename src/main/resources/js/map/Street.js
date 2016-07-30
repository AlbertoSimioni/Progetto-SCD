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

Street.prototype.changeColor = function(color,position){
    /*var sidewalk = null;
    if(position == "down"){
        sidewalk = this.sidewalks[0];
    }
    else if(position == "up"){
        sidewalk = this.sidewalks[1];
    }
    else if(position == "left"){
        sidewalk = this.sidewalks[0];
    }
    else if(position == "right"){
        sidewalk = this.sidewalks[1];
    }
    sidewalk.changeColor(color);*/
    this.sidewalks[position].changeColor(color);
};

Street.prototype.changeColorBusStop = function(color,position,entityType){
    if(this.isTripleLane()){
        this.changeColorTramStop(color,position,entityType)
    }
    else{
        if(entityType == "P") this.changeColor(color,position);
        else{
            if(position == "down"){
                this.lanes[0].changeColor(color);
            }
            else if(position == "up"){
                this.lanes[1].changeColor(color);
            }
            else if(position == "left"){
                this.lanes[0].changeColor(color);
            }
            else if(position == "right"){
                this.lanes[1].changeColor(color);
            }
        }
    }
}

Street.prototype.changeColorTramStop = function(color,position,entityType){
    var tramPosition = this.getTramPosition()
    if(entityType == "P"){ this.changeColor(color,position);}
    if(entityType=="T"){
        if(tramPosition == "up"){
            this.lanes[2].changeColor(color);
        }
        if(tramPosition == "down"){
            this.lanes[0].changeColor(color);
        }
        if(tramPosition == "left"){
            this.lanes[0].changeColor(color);
        }
        if(tramPosition == "right"){
            this.lanes[2].changeColor(color);
        }
    }
    if(entityType=="B" || entityType=="C"){
        if(tramPosition == "right" && position == "right"){
            this.lanes[1].changeColor(color);
        }
        if(tramPosition == "right" &&  position == "left"){
            this.lanes[0].changeColor(color);
        }
        if(tramPosition == "left" &&  position == "left"){
            this.lanes[1].changeColor(color);
        }
        if(tramPosition == "left" &&  position == "right"){
            this.lanes[2].changeColor(color);
        }
        if(tramPosition == "down" &&  position == "down"){
            this.lanes[1].changeColor(color);
        }
        if(tramPosition == "down" &&  position == "up"){
            this.lanes[2].changeColor(color);
        }
        if(tramPosition == "up" &&  position == "up"){
            this.lanes[1].changeColor(color);
        }
        if(tramPosition == "up" &&  position == "down"){
            this.lanes[0].changeColor(color);
        }
    }
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