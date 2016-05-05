/**
 * Created by artem on 21/04/15.
 * Lection about prototypes and inheritance.
 */

function dump(o) {
    for (var x in o) {
        console.log(x + ": " + o[x]);
    }
}

var Parent = {
    field: "qq"
};

function Point(x, y) {
    this.x = x;
    this.y = y;
}
Point.prototype = Parent;

var point = new Point(1, 2);

dump(point);