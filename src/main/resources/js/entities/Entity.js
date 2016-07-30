//CLASSE DI UNA ENTITà



//Costruttore
function Entity() {}

//aggiungo
Entity.prototype.show = function() {
    "use strict";
    this.shape.visible = true;
    //path è un campo che deve essere presente nelle classi che ereditano
};

//nasconde l'entità
Entity.prototype.hide = function() {
    "use strict";
    this.shape.visible = false;
};

//Funzione che disegna l'entità con lo stile dato in input
Entity.prototype.draw = function() {
    "use strict";
    //this.path = new Path[style.shape.type]({point: this.currentPosition,size : style.shape.size });
    //this.path.fillColor = style.color;
    //this.shape.myData = this;
    this.shape.onMouseEnter = this.myOnMouseEnter;
    // Create onMouseLeave event for dot
    this.shape.onMouseLeave = this.myOnMouseLeave;
    this.shape.onMouseDown = this.myOnMouseDown;
};

Entity.prototype.myOnMouseEnter = function(event) {
    "use strict";
    //var tooltipRect = new Rectangle(this.position + new Point(40, 40), new Size(100, 100));
    //this.tooltipLabel = new PointText(new Point(55,55));
    //this.tooltipLabel.fillColor = "white";
    //this.tooltipLabel.textColor = "blue";
    //this.tooltipLabel.strokeColor = "black";
    // Name the tooltip so we can retrieve it later
    //this.tooltipLabel.content = this.myData; //cercare questo campo//
    //this.tooltipLabel.bringToFront();
    // Add the tooltip to the parent (group)
    this.previousColor = this.fillColor
    this.fillColor = "blue";
};

Entity.prototype.myOnMouseLeave = function(event) {
    "use strict";
    // We retrieve the tooltip from its name in the parent node (group) then remove it
    //this.tooltipLabel.remove();
    this.fillColor = this.previousColor;
};

Entity.prototype.myOnMouseDown = function(event) {
    "use strict";
    // We retrieve the tooltip from its name in the parent node (group) then remove it
    //this.tooltipLabel.remove();
    var entityId = this.myData;
    console.log(entityId);
    if(lastEntityID != null && lastEntityID == entityId){
        colorSteps(lastPath,"oldColor");
        lastEntityID = null;
    }
    else{
        lastEntityID = entityId;
        webSocket.send("PATH-"+entityId);
    }
};



Entity.prototype.remove = function() {
    "use strict";
    if (this.shape)
        this.shape.remove();
};