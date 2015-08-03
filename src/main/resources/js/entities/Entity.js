//CLASSE DI UNA ENTITà


function EntitiesStyle() {
    "use strict";
    this.car = {
        shape: {
            type: "Rectangle",
            size: [2, 3]
        },
        color: "red"
    };
}


//Costruttore
function Entity() {}

//aggiungo
Entity.prototype.show = function() {
    "use strict";
    this.path.visible = true;
    //path è un campo che deve essere presente nelle classi che ereditano
};

//nasconde l'entità
Entity.prototype.hide = function() {
    "use strict";
    this.path.visible = false;
};

//Funzione che disegna l'entità con lo stile dato in input
Entity.prototype.draw = function() {
    "use strict";
    //this.path = new Path[style.shape.type]({point: this.currentPosition,size : style.shape.size });
    //this.path.fillColor = style.color;
    this.path.myData = this;
    this.path.onMouseEnter = this.myOnMouseEnter;
    // Create onMouseLeave event for dot
    this.path.onMouseLeave = this.myOnMouseLeave;
};

//funzione per quando il mouse passa sopra una entità
Entity.prototype.myOnMouseEnter = function(event) {
    "use strict";
    //var tooltipRect = new Rectangle(this.position + new Point(40, 40), new Size(100, 100));
    this.tooltipLabel = new PointText(this.position.subtract(new Point(-5, -5)));
    this.tooltipLabel.fillColor = "white";
    this.tooltipLabel.textColor = "blue";
    this.tooltipLabel.strokeColor = "black";
    // Name the tooltip so we can retrieve it later
    this.tooltipLabel.content = this.myData.id; //cercare questo campo//
    this.tooltipLabel.bringToFront();
    // Add the tooltip to the parent (group)
    this.fillColor = "green";
};

Entity.prototype.myOnMouseLeave = function(event) {
    "use strict";
    // We retrieve the tooltip from its name in the parent node (group) then remove it
    this.tooltipLabel.remove();
    this.fillColor = "red";
};

Entity.prototype.remove = function() {
    "use strict";
    if (this.path)
        this.path.remove();
};