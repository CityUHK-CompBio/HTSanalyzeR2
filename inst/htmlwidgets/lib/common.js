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

// Converts a #ffffff hex string into an [r,g,b] array
var h2r = function(hex) {
    var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
    return result ? [
        parseInt(result[1], 16),
        parseInt(result[2], 16),
        parseInt(result[3], 16)
    ] : null;
};

// Inverse of the above
var r2h = function(rgb) {
    return "#" + ((1 << 24) + (rgb[0] << 16) + (rgb[1] << 8) + rgb[2]).toString(16).slice(1);
};

var r2rgba = function(rgb, alpha) {
  return "rgba(" + rgb.join(",") + ", " + alpha + ")";
};

var h2rgba = function(hex, alpha) {
  return r2rgba(h2r(hex), alpha);
};

// Interpolates two [r,g,b] colors and returns an [r,g,b] of the result
// Taken from the awesome ROT.js roguelike dev library at
// https://github.com/ondras/rot.js
var _interpolateColor = function(color1, color2, factor) {
  if (arguments.length < 3) { factor = 0.5; }
  var result = color1.slice();
  for (var i=0;i<3;i++) {
    result[i] = Math.round(result[i] + factor*(color2[i]-color1[i]));
  }
  return result;
};

var _iterpolatePalette = function(palette, value) {
// {
//   domain:[0, 1],
//   range:["#9E1617", "#FFFFFF"]
// }
  var color1 = h2r(palette.range[0]);
  var color2 = h2r(palette.range[1]);

  var factor = 0;
  if(palette.domain[0] != palette.domain[1]) {
    factor = (value - palette.domain[0]) / (palette.domain[1] - palette.domain[0]);
    factor = factor < 0 ? 0 : (factor > 1 ? 1 : factor);
  }

  var rgb = _interpolateColor(color1, color2, factor);
  return r2h(rgb);
};


var forceGraphObj = null;

var registerForceGraph = function(global) {
  forceGraphObj = global;
}

var tabSwitched = function(tabId) {
  console.log("tab switched to " + tabId);
  if(forceGraphObj != null) {
    var el = $(tabId + " .forceGraph")[0];
    forceGraphObj.switchTab(el);
  }
}