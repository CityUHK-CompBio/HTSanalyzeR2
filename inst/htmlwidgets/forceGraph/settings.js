
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

