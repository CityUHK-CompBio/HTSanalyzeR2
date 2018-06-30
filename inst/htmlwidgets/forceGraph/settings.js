if(jQueryLoaded) {
  $.noConflict(true);
}

renderPalette = function(canvas, palette) {
    // width should be 100, height should be 1
    var width = 100;
    var context = canvas.getContext("2d");
    var image = context.createImageData(width, 1);

    for (var i = 0, j = -1, c; i < width; ++i) {
        c = _interpolateColor(palette.range, i / width);
        image.data[++j] = c[0];
        image.data[++j] = c[1];
        image.data[++j] = c[2];
        image.data[++j] = 255;
    }
    context.putImageData(image, 0, 0);
}

uniTextColors = function(scheme) {
    // scheme = "pos" / "neg"
    $("input#" + scheme + "Value0").css("color", $("input#" + scheme + "Color0").css("color"));
    $("input#" + scheme + "Value1").css("color", $("input#" + scheme + "Color1").css("color"));
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
    $("#layoutSwitches .checkbox input[value='layoutEdgeWeightInfluence']", panel).prop("checked", config.layout.edgeWeightInfluence != 0);


    $("#layoutGravity", panel).data("ionRangeSlider").update({from: config.layout.gravity});
    $("#layoutBarnesHutTheta", panel).data("ionRangeSlider").update({from: config.layout.barnesHutTheta});
    // $("#layoutEdgeWeightInfluence", panel).data("ionRangeSlider").update({from: config.layout.edgeWeightInfluence});
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
    $("#nodeColorNA", panel).colourpicker("value", config.node.NANodeColor);

    // Edge
    $("#edgeScale", panel).data("ionRangeSlider").update({from: config.edge.scale});
    $("#edgeColor", panel).colourpicker("value", config.edge.color);

    // ColorScheme
    $("#posColor0", panel).colourpicker("value", config.scheme.dual.pos.range[0]);
    $("#posColor1", panel).colourpicker("value", config.scheme.dual.pos.range[1]);
    $("#negColor0", panel).colourpicker("value", config.scheme.dual.neg.range[0]);
    $("#negColor1", panel).colourpicker("value", config.scheme.dual.neg.range[1]);
    $("input#posValue0", panel).prop("value", config.scheme.dual.pos.domain[0]);
    $("input#posValue1", panel).prop("value", config.scheme.dual.pos.domain[1]);
    $("input#negValue0", panel).prop("value", config.scheme.dual.neg.domain[0]);
    $("input#negValue1", panel).prop("value", config.scheme.dual.neg.domain[1]);

    renderPalette($("canvas#posPalette", panel)[0], config.scheme.dual.pos);
    renderPalette($("canvas#negPalette", panel)[0], config.scheme.dual.neg);
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
    $("#layoutSwitches .checkbox input[value='layoutEdgeWeightInfluence']").change(function() {
        handlers['edgeWeightInfluence'](this.checked ? 1 : 0);
    });

    $("#layoutGravity").on("change", function() {
        var value = $(this).prop("value");
        handlers["gravity"](value);
    });
    $("#layoutBarnesHutTheta").on("change", function() {
        var value = $(this).prop("value");
        handlers["barnesHutTheta"](value);
    });
    // $("#layoutEdgeWeightInfluence").on("change", function() {
    //     var value = $(this).prop("value");
    //     handlers["edgeWeightInfluence"](value);
    // });
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
    $("#nodeColorNA").on("change", function() {
        var value = $(this).colourpicker("value");
        handlers["nodeColorNA"](value);
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
        var val0 = parseFloat($("input#" + scheme + "Value0").prop("value"));
        var val1 = parseFloat($("input#" + scheme + "Value1").prop("value"));
        var color0 = $("#" + scheme + "Color0").colourpicker("value");
        var color1 = $("#" + scheme + "Color1").colourpicker("value");
        return {domain: [val0, val1], range: [color0, color1]};
    }
    $("#schemePanel input").on("change", function(evt) {
        // posValue1 posValue2 posColor1 posColor2
        // negValue1 negValue2 negColor1 negColor2
        var evtId = evt.currentTarget.id;
        var sch = evtId.substring(0,3) == "pos" ? "pos" : "neg";
        var type = evtId.substring(3,8) == "Value" ? "domain" : "range";
        var idx = parseInt(evtId.substring(8));
        var val = type == "domain" ? parseFloat($(evt.currentTarget).prop("value")) : $(evt.currentTarget).colourpicker("value");

        var palette = fetchSchemeValues(sch);
        renderPalette($("canvas#" + sch + "Palette")[0], palette);
        uniTextColors(sch);

        handlers["scheme"]("dual", sch, type, idx, val);
    })

    handlers['configured'] = true;
}

