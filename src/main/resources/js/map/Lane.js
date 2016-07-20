//Classe che rappresenta una corsia stradale
Lane.prototype.constructor = Lane;


//Costruttore
function Lane(id, from, to, isHorizontal, beginToEnd, tram,stop,drawStop) {
    "use strict";
    this.id = id;
    this.from = from;
    this.to = to;
    this.isHorizontal = isHorizontal;
    this.beginToEnd = beginToEnd;
    this.tram = tram;
    this.path = null;
    this.stop = stop;
    this.drawStop = drawStop;
};

//Campi statici
Lane.width = 6;
Lane.triangleRadius = 2;

//Disegna la corsia e il corrispondente triangolino per la direzione
Lane.prototype.draw = function() {
    this.path = new Path.Rectangle(this.from, this.to);
    if (this.tram)
        this.path.fillColor = "orange";
    else
        this.path.fillColor = "black";
    if(!this.stop){
        var triangle = null;
        //da qui fino alla fine codice per disegnare il triangolino che indica la direzione
        if (this.isHorizontal && this.beginToEnd) {
                triangle = new Path.RegularPolygon(
                new Point(this.to.x - Lane.triangleRadius, this.to.y + Lane.width / 2),
                 3,
                 Lane.triangleRadius
                 );
            triangle.rotate(90);
        }

        if (this.isHorizontal && (!this.beginToEnd)) {
                triangle = new Path.RegularPolygon(
                new Point(this.from.x + Lane.triangleRadius, this.from.y - Lane.width / 2),
                3,
                Lane.triangleRadius
                );
            triangle.rotate(270);
        }

        if ((!this.isHorizontal) && (!this.beginToEnd)) {
                triangle = new Path.RegularPolygon(
                new Point(this.from.x + Lane.width / 2, this.from.y - Lane.triangleRadius),
                 3,
                 Lane.triangleRadius
                 );
            triangle.rotate(180);
        }
        if ((!this.isHorizontal) && this.beginToEnd) {
            triangle = new Path.RegularPolygon(
                new Point(this.to.x - Lane.width / 2, this.to.y + Lane.triangleRadius),
                3,
                Lane.triangleRadius
                );
        }

        triangle.fillColor = "white";
    }
    if(this.drawStop){
        var text = null;
        if(this.isHorizontal) text = new PointText(new Point(this.from.x - (this.from.x  - this.to.x)/2 + 4, this.from.y));
        else text = new PointText(new Point(this.from.x +3, this.from.y - (this.from.y  - this.to.y)/2 + 4));
        text.style = {
            fontFamily: 'Courier New',
            fontWeight: 'bold',
            fontSize: 8,
            fillColor: 'white',
            justification: 'center'
        };

        text.content = 'B';
        if(this.tram) text.content = 'T';
    }
}