addCustomBtns = function(panel, state) {
    var dropdown = panel.find('div .dropdown');
    var menu = dropdown.find('.dropdown-menu');

    var createButton = function(icon, tooltip, func) {
        var control = $('<a data-func="customBtn"></a>');
        control.append('<i class="panel-control-icon ' + icon + '"></i>');
        control.append('<span class="control-title">' + tooltip + '</span>');
        control.attr('data-tooltip', tooltip);

        control.on('mousedown', function(ev) {
            ev.stopPropagation();
        });
        control.on('click', function(ev) {
            ev.stopPropagation();
            func();
        });

        return $('<li></li>').append(control);
    }

    // menu.prepend(createButton("glyphicon glyphicon-pause", "pause", state.controller.pause));
    // menu.prepend(createButton("glyphicon glyphicon-floppy-disk", "save", state.controller.saveImg));
}

appendPanelId = function(panel, state) {
    var panelId = state.elId;
    $("#accordion", panel).attr("id", "accordion" + panelId);

    $("[data-toggle='collapse']", panel).each(function() {
        $(this).attr("href", $(this).attr("href") + panelId);
        $(this).attr("data-parent", $(this).attr("data-parent") + panelId);
    });

    $(".panel-collapse", panel).each(function() {
        $(this).attr("id", $(this).attr("id") + panelId);
    });
}

renderPalette = function(canvas, domain, range) {
    // // width should be 100, height should be 1
    // var width = 100;
    // // var canvas = d3.select($("#nodeSchemes #" + schemeId + " #palette", panel)[0]);
    // var context = canvas.node().getContext("2d");
    // var image = context.createImageData(width, 1);

    // var max = Math.max.apply(null, domain);
    // var min = Math.min.apply(null, domain);
    // var domainMapping = domain.map(function(x) {return (x - min) * width / (max - min); });

    // var color = d3.scaleLinear()
    //     .domain(domainMapping)
    //     .range(range)
    //     .interpolate(d3.interpolateCubehelix.gamma(3.0));

    // for (var i = 0, j = -1, c; i < width; ++i) {
    //     c = d3.rgb(color(i));
    //     image.data[++j] = c.r;
    //     image.data[++j] = c.g;
    //     image.data[++j] = c.b;
    //     image.data[++j] = 255;
    // }
    // context.putImageData(image, 0, 0);
}

refreshValues = function(panel, state) {
    var curState = state[state.currentKey];
    // General
    $("#generalTitle", panel).val(curState.title);
    $('#generalTitleSize', panel).slider().slider('setValue', curState.titleSize)
    $("#generalLegendTitle", panel).val(curState.legendTitle);
    $('#generalDistance', panel).slider().slider('setValue', curState.distance)
    // Label
    var labelOptions = $("#labelOption", panel);
    labelOptions.find("label").removeClass("active");
    labelOptions.find("label[value='"+ curState.label +"']").addClass("active");
    $("#labelColor", panel)[0].jscolor.fromString(curState.labelColor);
    $('#labelOpacity', panel).slider().slider('setValue', curState.labelOpacity);
    $('#labelScale', panel).slider().slider('setValue', curState.labelScale);
    // Node
    var nodeOptions = $("#nodeShapeOption", panel);
    nodeOptions.find("label").removeClass("active");
    nodeOptions.find("label[value='"+ curState.nodeShape +"']").addClass("active");
    // Node - Scheme
    var scheme = $("#nodeSchemeBtns li a[value='" + curState.nodeScheme + "']", panel).text();
    var dropdownBtn = $('#nodeSchemeDropdown', panel);
    dropdownBtn.attr("value", curState.nodeScheme);
    dropdownBtn.html(scheme + ' <span class="caret"></span>')
    var canvas1 = d3.select($("#schemePreview1", panel)[0]);
    var canvas2 = d3.select($("#schemePreview2", panel)[0]);
    if(curState.nodeScheme == 'dual') {
        var palette1 = curState.palettes["dualPos"];
        var palette2 = curState.palettes["dualNeg"];
        renderPalette(canvas1, palette1.domain, palette1.range);
        renderPalette(canvas2, palette2.domain, palette2.range);
    } else {
        var palette = curState.palettes[curState.nodeScheme];
        renderPalette(canvas1, palette.domain, palette.range);
        renderPalette(canvas2, palette.domain, palette.range);
    }//
    $('#nodeScale', panel).slider().slider('setValue', curState.nodeScale);
    $("#nodeBorderColor", panel)[0].jscolor.fromString(curState.nodeBorderColor);
    $('#nodeBorderOpacity', panel).slider().slider('setValue', curState.nodeBorderOpacity);
    $('#nodeBorderWidth', panel).slider().slider('setValue', curState.nodeBorderWidth);
    // Edge
    $("#edgeColor", panel)[0].jscolor.fromString(curState.edgeColor);
    $('#edgeOpacity', panel).slider().slider('setValue', curState.edgeOpacity);
    $('#edgeScale', panel).slider().slider('setValue', curState.edgeScale);
    // ColorScheme
    var ids = ["linear2", "linear3", "dualPos", "dualNeg"];
    for(var i in ids) {
        var schemeId = ids[i];
        var palette = curState.palettes[schemeId];
        $("#nodeSchemes #" + schemeId + " #value1", panel).val(palette.domain[0]);
        $("#nodeSchemes #" + schemeId + " #value2", panel).val(palette.domain[1]);
        $("#nodeSchemes #" + schemeId + " #color1", panel)[0].jscolor.fromString(palette.range[0]);
        $("#nodeSchemes #" + schemeId + " #color2", panel)[0].jscolor.fromString(palette.range[1]);
        if(schemeId == "linear3") {
            $("#nodeSchemes #" + schemeId + " #value3", panel).val(palette.domain[2]);
            $("#nodeSchemes #" + schemeId + " #color3", panel)[0].jscolor.fromString(palette.range[2]);
        }
    }
}

initPanel = function(panel, title, state) {
    if("undefined" != typeof title) {
        $("#settingPanelTitle", panel).text("Settings (" + title + ")");
    }

    panel.lobiPanel({
        state: "collapsed",
        minWidth: 500,
        maxWidth: 1000,
        minHeight: 600,
        maxHeight: 800,

        reload: false,
        close: false,
        editTitle: false,
        expand: false,
        unpin: false,
        minimize: {
            tooltip: "Settings"
        },
    })

    var instance = panel.data('lobiPanel');
    instance.$el.attr("old-style", "left: 200px; top: 80px; z-index: 10001; position: fixed; width: 750px; right: auto; bottom: auto; height: 730px; user-select: initial;");

    instance.disableTooltips();
    instance.toggleMinimize = function() {
        if (instance.isMinimized()) {
            instance.maximize();
            instance.unpin();
        } else {
            instance.pin();
            instance.minimize();
        }
        return instance;
    };

    var decorator = function(funcName) {
        return function(obj) {
            state.controller[funcName](obj.value);
            // state.controller.refresh();
        }
    }

    // Layout
    $("#layoutLinLogMode", panel).change(function() {
        state.controller['linLogMode'](this.checked);
    });
    $("#layoutStrongGravityMode", panel).change(function() {
        state.controller['strongGravityMode'](this.checked);
    });
    $("#layoutOutboundAttractionDistribution", panel).change(function() {
        state.controller['outboundAttractionDistribution'](this.checked);
    });
    $("#layoutAdjustSizes", panel).change(function() {
        state.controller['adjustSizes'](this.checked);
    });
    $("#layoutBarnesHutOptimize", panel).change(function() {
        state.controller['barnesHutOptimize'](this.checked);
    });


    $('#layoutGravity', panel).slider().on('slide', decorator('gravity'));
    $('#layoutBarnesHutTheta', panel).slider().on('slide', decorator('barnesHutTheta'));
    $('#layoutEdgeWeightInfluence', panel).slider().on('slide', decorator('edgeWeightInfluence'));
    $('#layoutSlowDown', panel).slider().on('slide', decorator('slowDown'));


    // //General
    // $("#generalTitle", panel).change(function() {
    //     state.controller['title'](this.value);
    // });
    // $('#generalTitleSize', panel).slider().on('slide', decorator('titleSize'));
    // $("#generalLegendTitle", panel).change(function() {
    //     state.controller['legendTitle'](this.value);
    // });
    // $('#generalDistance', panel).slider().on('slide', decorator('distance'));

    // //Label
    // $("#labelOption :input", panel).change(function() {
    //     state.controller['labelOption'](this.value);
    // });
    // $("#labelColor", panel).change(function() {
    //     state.controller['labelColor']("#" + this.value);
    // });
    // $('#labelOpacity', panel).slider().on('slide', decorator('labelOpacity'));
    // $('#labelScale', panel).slider().on('slide', decorator('labelScale'));

    // // Node
    // $("#nodeShapeOption :input", panel).change(function() {
    //     state.controller['nodeShape'](this.value);
    // });
    // $("#nodeSchemeBtns li a", panel).click(function() {
    //     var selText = $(this).text();
    //     var schemeId = $(this).attr('value');
    //     var dropdownBtn = $("#nodeSchemeDropdown", panel);
    //     dropdownBtn.attr('value', schemeId);
    //     dropdownBtn.html(selText + ' <span class="caret"></span>');

    //     var canvas1 = d3.select($("#schemePreview1", panel)[0]);
    //     var canvas2 = d3.select($("#schemePreview2", panel)[0]);
    //     if(schemeId == 'dual') {
    //         var palette1 = fetchSchemeValues("dualPos");
    //         var palette2 = fetchSchemeValues("dualNeg");
    //         renderPalette(canvas1, palette1.domain, palette1.range);
    //         renderPalette(canvas2, palette2.domain, palette2.range);
    //     } else {
    //         var palette = fetchSchemeValues(schemeId);
    //         renderPalette(canvas1, palette.domain, palette.range);
    //         renderPalette(canvas2, palette.domain, palette.range);
    //     }
    //     state.controller.nodeScheme(schemeId);
    // });
    // $('#nodeScale', panel).slider().on('slide', decorator('nodeScale'));
    // $("#nodeBorderColor", panel).change(function() {
    //     state.controller['nodeBorderColor']('#' + this.value);
    // });
    // $('#nodeBorderOpacity', panel).slider().on('slide', decorator('nodeBorderOpacity'));
    // $('#nodeBorderWidth', panel).slider().on('slide', decorator('nodeBorderWidth'));

    // // Edge
    // $("#edgeColor", panel).change(function() {
    //     state.controller.edgeColor('#' + this.value);
    // });
    // $('#edgeOpacity', panel).slider().on('slide', decorator('edgeOpacity'));
    // $('#edgeScale', panel).slider().on('slide', decorator('edgeScale'));

    // // Scheme
    // var fetchSchemeValues = function(schemeId) {
    //     var val1 = parseFloat($("#nodeSchemes #" + schemeId + " #value1", panel).val());
    //     var val2 = parseFloat($("#nodeSchemes #" + schemeId + " #value2", panel).val());
    //     var color1 = "#" + $("#nodeSchemes #" + schemeId + " #color1", panel).val();
    //     var color2 = "#" + $("#nodeSchemes #" + schemeId + " #color2", panel).val();

    //     var dict = {domain: [val1, val2], range: [color1, color2]};
    //     if(schemeId == "linear3") {
    //         dict.domain.push(parseFloat($("#nodeSchemes #" + schemeId + " #value3", panel).val()));
    //         dict.range.push("#" + $("#nodeSchemes #" + schemeId + " #color3", panel).val());
    //     }
    //     return dict;
    // }

    // var uniTextColors = function(schemeId) {
    //     var txtColor1 = $("#nodeSchemes #" + schemeId + " #color1", panel).css("color");
    //     var txtColor2 = $("#nodeSchemes #" + schemeId + " #color2", panel).css("color");
    //     $("#nodeSchemes #" + schemeId + " #value1", panel).css("color", txtColor1);
    //     $("#nodeSchemes #" + schemeId + " #value2", panel).css("color", txtColor2);
    // }

    // var renderFunc = function(schemeId) {
    //     return function() {
    //         var values = fetchSchemeValues(schemeId);
    //         var canvas = d3.select($("#nodeSchemes #" + schemeId + " #palette", panel)[0]);
    //         state.controller.changeScheme(schemeId, values.domain, values.range);
    //         renderPalette(canvas, values.domain, values.range);
    //         uniTextColors(schemeId);

    //         if(schemeId.startsWith($("#nodeSchemeDropdown", panel).attr('value'))) {
    //             var canvas1 = d3.select($("#schemePreview1", panel)[0]);
    //             var canvas2 = d3.select($("#schemePreview2", panel)[0]);

    //             if(schemeId.startsWith("dual")) {
    //                 var palette1 = fetchSchemeValues("dualPos");
    //                 var palette2 = fetchSchemeValues("dualNeg");
    //                 renderPalette(canvas1, palette1.domain, palette1.range);
    //                 renderPalette(canvas2, palette2.domain, palette2.range);
    //             } else {
    //                 renderPalette(canvas1, values.domain, values.range);
    //                 renderPalette(canvas2, values.domain, values.range);
    //             }
    //         }
    //     }
    // }

    // var ids = ["linear2", "linear3", "dualPos", "dualNeg"];
    // for(var i in ids) {
    //     var schemeId = ids[i];
    //     var values = fetchSchemeValues(schemeId);
    //     var canvas = d3.select($("#nodeSchemes #" + schemeId + " #palette", panel)[0]);
    //     renderPalette(canvas, values.domain, values.range);
    //     uniTextColors(schemeId);
    //     $("#nodeSchemes #" + schemeId+ " input", panel).change(renderFunc(schemeId));
    // }

    addCustomBtns(panel, state);
    panel.removeClass("hidden");
}

configureSettingPanel = function(state) {
    var elParent = $('#' + state.elId).parent();
    var title = elParent.parents(".tab-pane").data("value");
    var panel = elParent.children(":first");
    var innerId = elParent.data("lobipanel-child-inner-id");
    if("undefined" != typeof innerId) {
        panel = $("[data-inner-id='" + innerId + "']")
    }

    // refreshValues(panel, state);

    var lobiInited = panel.hasClass('lobipanel') && "undefined" != typeof panel.data('inner-id');
    if(!lobiInited) {
        appendPanelId(panel, state);
        initPanel(panel, title, state);
    }
}
