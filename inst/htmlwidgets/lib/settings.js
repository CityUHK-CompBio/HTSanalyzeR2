addCustomBtns = function(panel, elState) {
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

    menu.prepend(createButton("glyphicon glyphicon-pause", "pause", elState.controller.pause));
    menu.prepend(createButton("glyphicon glyphicon-floppy-disk", "save", elState.controller.saveSvg));
}

appendPanelId = function(panel, elState) {
    var panelId = elState.elId;
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
    // width should be 100, height should be 1
    var width = 100;
    // var canvas = d3.select($("#nodeSchemes #" + schemeId + " #palette", panel)[0]);
    var context = canvas.node().getContext("2d");
    var image = context.createImageData(width, 1);

    var max = Math.max.apply(null, domain);
    var min = Math.min.apply(null, domain);
    var domainMapping = domain.map(function(x) {return (x - min) * width / (max - min); });

    var color = d3.scaleLinear()
        .domain(domainMapping)
        .range(range)
        .interpolate(d3.interpolateHcl);

    for (var i = 0, j = -1, c; i < width; ++i) {
        c = d3.rgb(color(i));
        image.data[++j] = c.r;
        image.data[++j] = c.g;
        image.data[++j] = c.b;
        image.data[++j] = 255;
    }
    context.putImageData(image, 0, 0);
}

refreshValues = function(panel, elState) {
    var curState = elState[elState.currentSubId];
    // General
    $("#generalTitle", panel).val(curState.title);
    $('#generalTitleSize', panel).slider().slider('setValue', curState.titleSize)
    $("#generalLegendTitle", panel).val(curState.legendTitle);
    $('#generalCharge', panel).slider().slider('setValue', curState.charge)
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
    var scheme = $("#nodeSchemeBtns li a[value='" + curState.nodeScheme + "']", panel).text();
    var dropdownBtn = $('#nodeSchemeDropdown', panel);
    dropdownBtn.attr("value", curState.nodeScheme);
    dropdownBtn.html(scheme + ' <span class="caret"></span>')
    var canvas = d3.select($("#schemePreview", panel)[0]);
    var palette = curState.palettes[curState.nodeScheme];
    renderPalette(canvas, palette.domain, palette.range);
    $('#nodeScale', panel).slider().slider('setValue', curState.nodeScale);
    $("#nodeBorderColor", panel)[0].jscolor.fromString(curState.nodeBorderColor);
    $('#nodeBorderOpacity', panel).slider().slider('setValue', curState.nodeBorderOpacity);
    $('#nodeBorderWidth', panel).slider().slider('setValue', curState.nodeBorderWidth);
    // Edge
    $("#edgeColor", panel)[0].jscolor.fromString(curState.edgeColor);
    $('#edgeOpacity', panel).slider().slider('setValue', curState.edgeOpacity);
    $('#edgeScale', panel).slider().slider('setValue', curState.edgeScale);
    // ColorScheme
    var ids = ["default", "scheme1", "scheme2"];
    for(var i in ids) {
        var schemeId = ids[i];
        var palette = curState.palettes[schemeId];
        $("#nodeSchemes #" + schemeId + " #value1", panel).val(palette.domain[0]);
        $("#nodeSchemes #" + schemeId + " #value2", panel).val(palette.domain[1]);
        $("#nodeSchemes #" + schemeId + " #value3", panel).val(palette.domain[2]);
        $("#nodeSchemes #" + schemeId + " #color1", panel)[0].jscolor.fromString(palette.range[0]);
        $("#nodeSchemes #" + schemeId + " #color2", panel)[0].jscolor.fromString(palette.range[1]);
        $("#nodeSchemes #" + schemeId + " #color3", panel)[0].jscolor.fromString(palette.range[2]);
    }
}

initPanel = function(panel, title, elState) {
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
            elState.controller[funcName](obj.value)
        }
    }

    //General
    $("#generalTitle", panel).change(function() {
        elState.controller['title'](this.value);
    });
    $('#generalTitleSize', panel).slider().on('slide', decorator('titleSize'));
    $("#generalLegendTitle", panel).change(function() {
        elState.controller['legendTitle'](this.value);
    });
    $('#generalCharge', panel).slider().on('slide', decorator('charge'));
    $('#generalDistance', panel).slider().on('slide', decorator('distance'));

    //Label
    $("#labelOption :input", panel).change(function() {
        elState.controller['labelOption'](this.value);
    });
    $("#labelColor", panel).change(function() {
        elState.controller['labelColor']("#" + this.value);
    });
    $('#labelOpacity', panel).slider().on('slide', decorator('labelOpacity'));
    $('#labelScale', panel).slider().on('slide', decorator('labelScale'));

    // Node
    $("#nodeShapeOption :input", panel).change(function() {
        elState.controller['nodeShape'](this.value);
    });
    $("#nodeSchemeBtns li a", panel).click(function() {
        var selText = $(this).text();
        var schemeId = $(this).attr('value');
        var palette = fetchSchemeValues(schemeId);
        var canvas = d3.select($("#schemePreview", panel)[0]);
        var dropdownBtn = $("#nodeSchemeDropdown", panel);
        dropdownBtn.attr('value', schemeId);
        dropdownBtn.html(selText + ' <span class="caret"></span>');
        renderPalette(canvas, palette.domain, palette.range);
        elState.controller.nodeScheme(schemeId);
    });
    $('#nodeScale', panel).slider().on('slide', decorator('nodeScale'));
    $("#nodeBorderColor", panel).change(function() {
        elState.controller['nodeBorderColor']('#' + this.value);
    });
    $('#nodeBorderOpacity', panel).slider().on('slide', decorator('nodeBorderOpacity'));
    $('#nodeBorderWidth', panel).slider().on('slide', decorator('nodeBorderWidth'));

    // Edge
    $("#edgeColor", panel).change(function() {
        elState.controller.edgeColor('#' + this.value);
    });
    $('#edgeOpacity', panel).slider().on('slide', decorator('edgeOpacity'));
    $('#edgeScale', panel).slider().on('slide', decorator('edgeScale'));

    // Scheme
    var fetchSchemeValues = function(schemeId) {
        var val1 = parseFloat($("#nodeSchemes #" + schemeId + " #value1", panel).val());
        var val2 = parseFloat($("#nodeSchemes #" + schemeId + " #value2", panel).val());
        var val3 = parseFloat($("#nodeSchemes #" + schemeId + " #value3", panel).val());
        var color1 = "#" + $("#nodeSchemes #" + schemeId + " #color1", panel).val();
        var color2 = "#" + $("#nodeSchemes #" + schemeId + " #color2", panel).val();
        var color3 = "#" + $("#nodeSchemes #" + schemeId + " #color3", panel).val();

        return {domain: [val1, val2, val3], range: [color1, color2, color3]};
    }

    var renderFunc = function(schemeId) {
        return function() {
            var values = fetchSchemeValues(schemeId);
            var canvas = d3.select($("#nodeSchemes #" + schemeId + " #palette", panel)[0]);
            elState.controller.changeScheme(schemeId, values.domain, values.range);
            renderPalette(canvas, values.domain, values.range);

            if(schemeId == $("#nodeSchemeDropdown", panel).attr('value')) {
                var preview = d3.select($("#schemePreview", panel)[0]);
                renderPalette(preview, values.domain, values.range);
            }
        }
    }

    var ids = ["default", "scheme1", "scheme2"];
    for(var i in ids) {
        var schemeId = ids[i];
        var values = fetchSchemeValues(schemeId);
        var canvas = d3.select($("#nodeSchemes #" + schemeId + " #palette", panel)[0]);
        renderPalette(canvas, values.domain, values.range);
        $("#nodeSchemes #" + schemeId+ " input", panel).change(renderFunc(schemeId));
    }

    addCustomBtns(panel, elState);
    panel.removeClass("hidden");
}

configureSettingPanel = function(elState) {
    var elParent = $('#' + elState.elId).parent();
    var title = elParent.parents(".tab-pane").data("value");
    var panel = elParent.children(":first");
    var innerId = elParent.data("lobipanel-child-inner-id");
    if("undefined" != typeof innerId) {
        panel = $("[data-inner-id='" + innerId + "']")
    }

    refreshValues(panel, elState);

    var lobiInited = panel.hasClass('lobipanel') && "undefined" != typeof panel.data('inner-id');
    if(!lobiInited) {
        appendPanelId(panel, elState);
        initPanel(panel, title, elState);
    }
}
