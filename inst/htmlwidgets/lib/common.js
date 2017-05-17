d3ForceCalc = {
    triangle: [
        [0, -1.538],
        [1.33, 0.77],
        [-1.33, 0.77]
    ],

    rectangle: [
        [-1, -1],
        [1, -1],
        [1, 1],
        [-1, 1]
    ],

    diamond: [
        [0, -1.25],
        [1.25, 0],
        [0, 1.25],
        [-1.25, 0]
    ],

    circle: [],

    points: function(shape, factor) {
        var pts = d3ForceCalc[shape];
        pts = pts.map(function(arr) {
            return arr.map(function(x) {
                return x * factor;
            });
        });
        return pts.join(' ');
    }
};


// http://werxltd.com/wp/2010/05/13/javascript-implementation-of-javas-string-hashcode-method/
String.prototype.hashCode = function() {
  var hash = 0, i, chr;
  if (this.length === 0) return hash;
  for (i = 0; i < this.length; i++) {
    chr   = this.charCodeAt(i);
    hash  = ((hash << 5) - hash) + chr;
    hash |= 0; // Convert to 32bit integer
  }
  return hash;
};
