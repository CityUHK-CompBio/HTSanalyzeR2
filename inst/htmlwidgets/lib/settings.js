// addCustomBtns = function(panel) {
//     var dropdown = panel.find('div .dropdown');
//     var menu = dropdown.find('.dropdown-menu');

//     var createButton = function(icon, tooltip) {
//         var control = $('<a data-func="customBtn" id="customBtn' + tooltip + '" ></a>');
//         control.append('<i class="panel-control-icon ' + icon + '"></i>');
//         control.append('<span class="control-title">' + tooltip + '</span>');
//         control.attr('data-tooltip', tooltip);
//         control.on('mousedown', function(ev) {
//             ev.stopPropagation();
//         });
//         control.on('click', function(ev) {
//             ev.stopPropagation();
//             // func();
//         });

//         return $('<li></li>').append(control);
//     }

//     menu.prepend(createButton("glyphicon glyphicon-pause", "Pause"));
//     menu.prepend(createButton("glyphicon glyphicon-refresh", "Refresh"));
//     menu.prepend(createButton("glyphicon glyphicon-floppy-disk", "Save"));
// }

// renderPalette = function(canvas, domain, range) {
//     // width should be 100, height should be 1
//     var width = 100;
//     var context = canvas.getContext("2d");
//     var image = context.createImageData(width, 1);

//     var interpolate = function (factor) {
//         return _interpolateColor(h2r(range[0]), h2r(range[1]), factor);
//     }

//     for (var i = 0, j = -1, c; i < width; ++i) {
//         c = interpolate(i / width);
//         image.data[++j] = c[0];
//         image.data[++j] = c[1];
//         image.data[++j] = c[2];
//         image.data[++j] = 255;
//     }
//     context.putImageData(image, 0, 0);
// }

// refreshValues = function(panel, state) {
//     var config = state[state.currentKey];
//     // Layout
//     $("#layoutLinLogMode", panel).prop("checked", config.layout.linLogMode);
//     $("#layoutStrongGravityMode", panel).prop("checked", config.layout.strongGravityMode);
//     $("#layoutOutboundAttractionDistribution", panel).prop("checked", config.layout.outboundAttractionDistribution);
//     $("#layoutAdjustSizes", panel).prop("checked", config.layout.adjustSizes);
//     $("#layoutBarnesHutOptimize", panel).prop("checked", config.layout.barnesHutOptimize);

//     $('#layoutGravity', panel).slider().slider('setValue', config.layout.gravity);
//     $('#layoutBarnesHutTheta', panel).slider().slider('setValue', config.layout.barnesHutTheta);
//     $('#layoutEdgeWeightInfluence', panel).slider().slider('setValue', config.layout.edgeWeightInfluence);
//     $('#layoutSlowDown', panel).slider().slider('setValue', config.layout.slowDown);

//     // Label
//     var labelOptions = $("#labelOption", panel);
//     labelOptions.find("label").removeClass("active");
//     // labelOptions.find("label[value='"+ curState.label +"']").addClass("active");
//     $("#labelColor", panel)[0].jscolor.fromString(config.label.color);
//     $('#labelOpacity', panel).slider().slider('setValue', config.label.opacity);
//     $('#labelScale', panel).slider().slider('setValue', config.label.scale);

//     // Node
//     $('#nodeScale', panel).slider().slider('setValue', config.node.scale);
//     $('#nodeOpacity', panel).slider().slider('setValue', config.node.opacity);
//     $("#nodeBorderColor", panel)[0].jscolor.fromString(config.node.borderColor);
//     $('#nodeBorderOpacity', panel).slider().slider('setValue', config.node.borderOpacity);
//     $('#nodeBorderWidth', panel).slider().slider('setValue', config.node.borderWidth);
    
//     // Edge
//     $("#edgeColor", panel)[0].jscolor.fromString(config.edge.color);
//     $('#edgeOpacity', panel).slider().slider('setValue', config.edge.opacity);
//     $('#edgeScale', panel).slider().slider('setValue', config.edge.scale);

//     // ColorScheme
//     var ids = ["Pos", "Neg"];
//     for(var i in ids) {
//         var schemeId = ids[i];
//         var palette = config.scheme.dual[schemeId];
//         $("#nodeSchemes #dual" + schemeId + " #value1", panel).val(palette.domain[0]);
//         $("#nodeSchemes #dual" + schemeId + " #value2", panel).val(palette.domain[1]);
//         $("#nodeSchemes #dual" + schemeId + " #color1", panel)[0].jscolor.fromString(palette.range[0]);
//         $("#nodeSchemes #dual" + schemeId + " #color2", panel)[0].jscolor.fromString(palette.range[1]);
//     }
// }

// appendPanelId = function(panel, panelId) {
//     $("#accordion", panel).attr("id", "accordion" + panelId);

//     $("[data-toggle='collapse']", panel).each(function() {
//         $(this).attr("href", $(this).attr("href") + panelId);
//         $(this).attr("data-parent", $(this).attr("data-parent") + panelId);
//     });

//     $(".panel-collapse", panel).each(function() {
//         $(this).attr("id", $(this).attr("id") + panelId);
//     });
// }

// initPanel = function(panel, title) {
//     if("undefined" != typeof title) {
//         $("#settingPanelTitle", panel).text("Settings (" + title + ")");
//     }

//     panel.lobiPanel({
//         state: "collapsed",
//         minWidth: 400,
//         maxWidth: 600,
//         minHeight: 600,
//         maxHeight: 800,

//         reload: false,
//         close: false,
//         editTitle: false,
//         expand: false,
//         unpin: false,
//         minimize: {
//             tooltip: "Settings"
//         },
//     })

//     var instance = panel.data('lobiPanel');
//     instance.$el.attr("old-style", "left: 200px; top: 80px; z-index: 10001; position: fixed; width: 750px; right: auto; bottom: auto; height: 730px; user-select: initial;");

//     instance.disableTooltips();
//     instance.toggleMinimize = function() {
//         if (instance.isMinimized()) {
//             instance.maximize();
//             instance.unpin();
//         } else {
//             instance.pin();
//             instance.minimize();
//         }
//         return instance;
//     };

//     addCustomBtns(panel);
//     panel.removeClass("hidden");
// }

// configureSettingPanel = function(state) {
//     var panelId = state.elId;
//     var elParent = $('#' + panelId).parent();
//     var title = elParent.parents(".tab-pane").data("value");
//     var panel = elParent.children(":first");
//     var innerId = elParent.data("lobipanel-child-inner-id");
//     if("undefined" != typeof innerId) {
//         panel = $("[data-inner-id='" + innerId + "']")
//     }

//     var lobiInited = panel.hasClass('lobipanel') && "undefined" != typeof panel.data('inner-id');
//     if(!lobiInited) {
//         appendPanelId(panel, panelId);
//         initPanel(panel, title);
//     }

//     // refreshValues(panel, state);
//     refreshListeners(panel, state);
// }

// refreshSettingPanel = function(state) {
//     var panelId = state.elId;
//     var elParent = $('#' + panelId).parent();
//     var title = elParent.parents(".tab-pane").data("value");
//     var panel = elParent.children(":first");

//     refreshValues(panel, state);
// }

renderPalette = function(canvas, palette) {
    // width should be 100, height should be 1
    var width = 100;
    var context = canvas.getContext("2d");
    var image = context.createImageData(width, 1);

    for (var i = 0, j = -1, c; i < width; ++i) {
        c = _iterpolateColor(palette.range, i / width);
        image.data[++j] = c[0];
        image.data[++j] = c[1];
        image.data[++j] = c[2];
        image.data[++j] = 255;
    }
    context.putImageData(image, 0, 0);
}

uniTextColors = function(scheme) {
    // scheme = "pos" / "neg"
    $("input#" + scheme + "Value1").css("color", $("input#" + scheme + "Color1").css("color"));
    $("input#" + scheme + "Value2").css("color", $("input#" + scheme + "Color2").css("color"));
}

updateShinyInput = function(panel, id, val) {
    // ChangeValue via shiny. General but not robust.
    var element = $(".shiny-bound-input#" + id, panel);
    var data = element.data("shiny-input-binding");
    if (element.length > 0) {
        data.receiveMessage(element[0], {value: val})
    }
}

refreshValues = function(panel, config) {
    console.log("refresing values");
    // Layout
    $("#layoutSwitches .checkbox input[value='layoutLinLogMode']", panel).prop("checked", config.layout.linLogMode);
    $("#layoutSwitches .checkbox input[value='layoutStrongGravityMode']", panel).prop("checked", config.layout.strongGravityMode);
    $("#layoutSwitches .checkbox input[value='layoutOutboundAttractionDistribution']", panel).prop("checked", config.layout.outboundAttractionDistribution);
    $("#layoutSwitches .checkbox input[value='layoutAdjustSizes']", panel).prop("checked", config.layout.adjustSizes);
    $("#layoutSwitches .checkbox input[value='layoutBarnesHutOptimize']", panel).prop("checked", config.layout.barnesHutOptimize);

    $("#layoutGravity", panel).data("ionRangeSlider").update({from: config.layout.gravity});
    $("#layoutBarnesHutTheta", panel).data("ionRangeSlider").update({from: config.layout.barnesHutTheta});
    $("#layoutEdgeWeightInfluence", panel).data("ionRangeSlider").update({from: config.layout.edgeWeightInfluence});
    $("#layoutSlowDown", panel).data("ionRangeSlider").update({from: config.layout.slowDown});

    // Label
    $("#labelOption input[value='" + config.label.text + "']", panel).prop("checked", true);
    $("#labelScale", panel).data("ionRangeSlider").update({from: config.label.scale});
    $("#labelColor", panel).colourpicker("value", config.label.color);
    
    // Node
    $("#nodeScale", panel).data("ionRangeSlider").update({from: config.node.scale});
    $("#nodeOpacity", panel).data("ionRangeSlider").update({from: config.node.opacity});
    $("#nodeBorderWidth", panel).data("ionRangeSlider").update({from: config.node.borderWidth});
    $("#nodeBorderColor", panel).colourpicker("value", config.node.borderColor);

    // Edge
    $("#edgeScale", panel).data("ionRangeSlider").update({from: config.edge.scale});
    $("#edgeColor", panel).colourpicker("value", config.edge.color);

    // ColorScheme
    $("#posColor1", panel).colourpicker("value", config.scheme.dual.Pos.range[0]);
    $("#posColor2", panel).colourpicker("value", config.scheme.dual.Pos.range[1]);
    $("#negColor1", panel).colourpicker("value", config.scheme.dual.Neg.range[0]);
    $("#negColor2", panel).colourpicker("value", config.scheme.dual.Neg.range[1]);
    $("input#posValue1", panel).prop("value", config.scheme.dual.Pos.domain[0]);
    $("input#posValue2", panel).prop("value", config.scheme.dual.Pos.domain[1]);
    $("input#negValue1", panel).prop("value", config.scheme.dual.Neg.domain[0]);
    $("input#negValue2", panel).prop("value", config.scheme.dual.Neg.domain[1]);

    renderPalette($("canvas#posPalette", panel)[0], config.scheme.dual.Pos);
    renderPalette($("canvas#negPalette", panel)[0], config.scheme.dual.Neg);
    uniTextColors("pos");
    uniTextColors("neg");
}

refreshSettingPanel = function(state, config) {
    // var elParent = $('#' + panelId).parent();
    // var title = elParent.parents(".tab-pane").data("value");
    // var panel = elParent.children(":first");

    var panel = $("#settingBar");
    refreshValues(panel, config);
}

configureSettingHandlers = function(handlers) {
    if(handlers.hasOwnProperty('configured')) {
        return;
    }

    // Layout
    $("#layoutSwitches .checkbox input[value='layoutLinLogMode']").change(function() {
        handlers['linLogMode'](this.checked);
    });
    $("#layoutSwitches .checkbox input[value='layoutStrongGravityMode']").change(function() {
        handlers['strongGravityMode'](this.checked);
    });
    $("#layoutSwitches .checkbox input[value='layoutOutboundAttractionDistribution']").change(function() {
        handlers['outboundAttractionDistribution'](this.checked);
    });
    $("#layoutSwitches .checkbox input[value='layoutAdjustSizes']").change(function() {
        handlers['adjustSizes'](this.checked);
    });
    $("#layoutSwitches .checkbox input[value='layoutBarnesHutOptimize']").change(function() {
        handlers['barnesHutOptimize'](this.checked);
    });

    $("#layoutGravity").on("change", function() {
        var value = $(this).prop("value");
        handlers["gravity"](value);
    });
    $("#layoutBarnesHutTheta").on("change", function() {
        var value = $(this).prop("value");
        handlers["barnesHutTheta"](value);
    });
    $("#layoutEdgeWeightInfluence").on("change", function() {
        var value = $(this).prop("value");
        handlers["edgeWeightInfluence"](value);
    });
    $("#layoutSlowDown").on("change", function() {
        var value = $(this).prop("value");
        handlers["slowDown"](value);
    });

    // Label
    $("#labelOption input").change(function(ev) {
        var value = ev.currentTarget.value;
        handlers["labelOption"](value);
    });
    $("#labelScale").on("change", function() {
        var value = $(this).prop("value");
        handlers["labelScale"](value);
    });
    $("#labelColor").on("change", function() {
        var value = $(this).colourpicker("value");
        handlers["labelColor"](value);
    });

    // Node
    $("#nodeScale").on("change", function() {
        var value = $(this).prop("value");
        handlers["nodeScale"](value);
    });
    $("#nodeOpacity").on("change", function() {
        var value = $(this).prop("value");
        handlers["nodeOpacity"](value);
    });
    $("#nodeBorderWidth").on("change", function() {
        var value = $(this).prop("value");
        handlers["nodeBorderWidth"](value);
    });
    $("#nodeBorderColor").on("change", function() {
        var value = $(this).colourpicker("value");
        handlers["nodeBorderColor"](value);
    });
    
    // Edge
    $("#edgeScale").on("change", function() {
        var value = $(this).prop("value");
        handlers["edgeScale"](value);
    });
    $("#edgeColor").on("change", function() {
        var value = $(this).colourpicker("value");
        handlers["edgeColor"](value);
    });

    // ColorScheme
    var fetchSchemeValues = function(scheme) {
        // scheme = "pos" / "neg"
        var val1 = parseFloat($("input#" + scheme + "Value1").prop("value"));
        var val2 = parseFloat($("input#" + scheme + "Value2").prop("value"));
        var color1 = $("#" + scheme + "Color1").colourpicker("value");
        var color2 = $("#" + scheme + "Color2").colourpicker("value");
        return {domain: [val1, val2], range: [color1, color2]};
    }
    $("#dualPos input").on("change", function() {
        var palette = fetchSchemeValues("pos");
        renderPalette($("canvas#posPalette")[0], palette);
        uniTextColors("pos");
        handlers["scheme"]("dualPos", palette.domain, palette.range);
    })    
    $("#dualNeg input").on("change", function() {
        var palette = fetchSchemeValues("neg");
        renderPalette($("canvas#negPalette")[0], palette);
        uniTextColors("neg");
        handlers["scheme"]("dualNeg", palette.domain, palette.range);
    })

    handlers['configured'] = true;
}


// refreshListeners = function(panel, state) {
//     var decorator = function(funcName) {
//         return function(obj) {
//             state.controllers[funcName](obj.value);
//         }
//     }

//     // Layout
//     $("#layoutLinLogMode", panel).change(function() {
//         state.controllers['linLogMode'](this.checked);
//     });
//     $("#layoutStrongGravityMode", panel).change(function() {
//         state.controllers['strongGravityMode'](this.checked);
//     });
//     $("#layoutOutboundAttractionDistribution", panel).change(function() {
//         state.controllers['outboundAttractionDistribution'](this.checked);
//     });
//     $("#layoutAdjustSizes", panel).change(function() {
//         state.controllers['adjustSizes'](this.checked);
//     });
//     $("#layoutBarnesHutOptimize", panel).change(function() {
//         state.controllers['barnesHutOptimize'](this.checked);
//     });
//     $('#layoutGravity', panel).slider().on('slide', decorator('gravity'));
//     $('#layoutBarnesHutTheta', panel).slider().on('slide', decorator('barnesHutTheta'));
//     $('#layoutEdgeWeightInfluence', panel).slider().on('slide', decorator('edgeWeightInfluence'));
//     $('#layoutSlowDown', panel).slider().on('slide', decorator('slowDown'));


//     //Label
//     $("#labelOption :input", panel).change(function() { state.controllers['labelOption'](this.value) });
//     $("#labelColor", panel).change(function() { state.controllers['labelColor']("#" + this.value) });
//     $('#labelOpacity', panel).slider().on('slide', decorator('labelOpacity'));
//     $('#labelScale', panel).slider().on('slide', decorator('labelScale'));

//     // Node
//     $('#nodeScale', panel).slider().on('slide', decorator('nodeScale'));
//     $('#nodeOpacity', panel).slider().on('slide', decorator('nodeOpacity'));
//     $("#nodeBorderColor", panel).change(function() { state.controllers['nodeBorderColor']("#" + this.value) });
//     $('#nodeBorderOpacity', panel).slider().on('slide', decorator('nodeBorderOpacity'));
//     $('#nodeBorderWidth', panel).slider().on('slide', decorator('nodeBorderWidth'));

//     // Edge
//     $("#edgeColor", panel).change(function() { state.controllers.edgeColor('#' + this.value) });
//     $('#edgeOpacity', panel).slider().on('slide', decorator('edgeOpacity'));
//     $('#edgeScale', panel).slider().on('slide', decorator('edgeScale'));

//     // Scheme
//     var fetchSchemeValues = function(schemeId) {
//         var val1 = parseFloat($("#nodeSchemes #" + schemeId + " #value1", panel).val());
//         var val2 = parseFloat($("#nodeSchemes #" + schemeId + " #value2", panel).val());
//         var color1 = "#" + $("#nodeSchemes #" + schemeId + " #color1", panel).val();
//         var color2 = "#" + $("#nodeSchemes #" + schemeId + " #color2", panel).val();
//         return {domain: [val1, val2], range: [color1, color2]};
//     }

//     var uniTextColors = function(schemeId) {
//         var txtColor1 = $("#nodeSchemes #" + schemeId + " #color1", panel).css("color");
//         var txtColor2 = $("#nodeSchemes #" + schemeId + " #color2", panel).css("color");
//         $("#nodeSchemes #" + schemeId + " #value1", panel).css("color", txtColor1);
//         $("#nodeSchemes #" + schemeId + " #value2", panel).css("color", txtColor2);
//     }

//     var renderFunc = function(schemeId) {
//         return function() {
//             var values = fetchSchemeValues(schemeId);
//             var canvas = $("#nodeSchemes #" + schemeId + " #palette", panel)[0];
//             renderPalette(canvas, values.domain, values.range);
//             uniTextColors(schemeId);
//             state.controllers.scheme(schemeId, values.domain, values.range);
//         }
//     }

//     var ids = ["dualPos", "dualNeg"];
//     for(var i in ids) {
//         var schemeId = ids[i];
//         var values = fetchSchemeValues(schemeId);
//         var canvas = $("#nodeSchemes #" + schemeId + " #palette", panel)[0];
//         renderPalette(canvas, values.domain, values.range);
//         uniTextColors(schemeId);
//         $("#nodeSchemes #" + schemeId+ " input", panel).change(renderFunc(schemeId));
//     }

//     // CustomButtons
//     $("#customBtnPause", panel).on('click', function(ev) { 
//         ev.stopPropagation(); 
//         state.controllers.pause(); 
//     });
//     $("#customBtnRefresh", panel).on('click', function(ev) { 
//         ev.stopPropagation(); 
//         state.controllers.refresh(); 
//     });
//     $("#customBtnSave", panel).on('click', function(ev) { 
//         ev.stopPropagation(); 
//         state.controllers.saveSVG(); 
//     });
// }

