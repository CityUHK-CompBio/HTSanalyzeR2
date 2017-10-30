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

var forceGraphObj = null;

var registerForceGraph = function(global) {
  forceGraphObj = global;
}

var tabSwitched = function(tabId) {
  // console.log("tab switched to " + tabId);
  if(forceGraphObj != null) {
    var el = $(tabId + " .forceGraph")[0];
    forceGraphObj.switchTab(el);
  }
}



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

// range: ["#9E1617", "#FFFFFF"] }, factor: 0-1
// return: rgb array
var _iterpolateColor = function(range, factor) {
  var color1 = h2r(range[0]);
  var color2 = h2r(range[1]);
  var rgb = color1.slice();
  for (var i=0; i<3; i++) {
    rgb[i] = Math.round(rgb[i] + factor*(color2[i]-color1[i]));
  }

  return rgb;
}

// Taken from the awesome ROT.js roguelike dev library at
// https://github.com/ondras/rot.js
// palette: { domain:[0, 1], range:["#9E1617", "#FFFFFF"] }, opacity: 0-1
// return: rgba str;
var _iterpolatePalette = function(palette, value, opacity) {
  var factor = 0;
  if(palette.domain[0] != palette.domain[1]) {
    factor = (value - palette.domain[0]) / (palette.domain[1] - palette.domain[0]);
    factor = factor < 0 ? 0 : (factor > 1 ? 1 : factor);
  }

  var rgba = _iterpolateColor(palette.range, factor);
  rgba.push(opacity);
  return "rgba(" + rgba.join(",") + ")";
};

var hex2rgba = function(hex) {
  var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})?[a-f\d]*$/i.exec(hex + "ff");
  var rgba = [0, 0, 0, 1];
  if (result) {
    rgba = [parseInt(result[1], 16), parseInt(result[2], 16), parseInt(result[3], 16), parseInt(result[4], 16) / 255.0];
  }
  return "rgba(" + rgba.join(",") + ")";
}
