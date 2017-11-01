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



/// Connect forceGraph.js and setting panel.
var forceGraphObj = null;
var registerForceGraph = function(global) {
  forceGraphObj = global;
}

var tabSwitched = function(tabId) {
  // console.log("tab switched to " + tabId);
  if(forceGraphObj != null) {
    var el = $(tabId + " .forceGraph")[0];
    el && forceGraphObj.switchTab(el);
  }
}

var forceGraphFuncs = function(funcId) {
  if(forceGraphObj != null) {
    var handlers = forceGraphObj.store["handlers"];
    if(handlers && handlers[funcId]) {
      handlers[funcId]();
    } 
  }
}


/// Custom the dashboard framework.
var initReportFramework = function() {
  // Add Custom Btns
  var pauseBtn = $("<div/>", { class: "cust-setting-btn cust-btn-pause" }).append(
    $("<i/>", { class:"fa fa-pause", status: "playing"})
  );
  var saveBtn = $("<div/>", { class: "cust-setting-btn cust-btn-save" }).append(
    $("<i/>", { class:"fa fa-save"})
  );
  $(".box:has(.forceGraph) > .box-header").append(saveBtn).append(pauseBtn);

  // Active first tab
  var tabId = $('section.sidebar ul.sidebar-menu li a:first-child').attr("href");
  $(".tab-content .tab-pane" + tabId).addClass("active");

  // Add Listeners
  $('li.messages-menu').click(function (ev) {
    ev.stopPropagation();
    $('#settingBar').toggleClass('active');
  });

  $('section.sidebar ul.sidebar-menu li').click(function(ev) {
    $(".tab-content .tab-pane.active").removeClass("active");
    var tabId = $("a", ev.currentTarget).attr("href");
    $(".tab-content .tab-pane" + tabId).addClass("active");

    $('li.messages-menu').css("display", tabId == "#shiny-tab-table_tab" ? "none" : "block");
    $('#settingBar').removeClass('active');

    tabSwitched(tabId);
  });

  $('.cust-btn-pause').click(function(ev) {
    var icon = $("i", ev.currentTarget);
    if(icon.attr("status") == "playing") {
      icon.removeClass("fa-pause").addClass("fa-play").attr("status", "pausing");
      forceGraphFuncs("pause");
    } else {
      icon.removeClass("fa-play").addClass("fa-pause").attr("status", "playing");
      forceGraphFuncs("refresh");
    }
  });
  $('.cust-btn-save').click(function(ev) {
    forceGraphFuncs("saveSVG");
  });

}


$(function() {
  initReportFramework();
});



