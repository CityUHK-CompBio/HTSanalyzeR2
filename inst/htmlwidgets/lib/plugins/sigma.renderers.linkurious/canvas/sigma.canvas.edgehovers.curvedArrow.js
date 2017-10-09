;(function() {
  'use strict';

  sigma.utils.pkg('sigma.canvas.edgehovers');

  /**
   * This hover renderer will display the edge with a different color or size.
   * It will also display the label with a background.
   *
   * @param  {object}                   edge         The edge object.
   * @param  {object}                   source node  The edge source node.
   * @param  {object}                   target node  The edge target node.
   * @param  {CanvasRenderingContext2D} context      The canvas context.
   * @param  {configurable}             settings     The settings function.
   */
  sigma.canvas.edgehovers.curvedArrow =
    function(edge, source, target, context, settings) {
    var color = edge.active ?
          edge.active_color || settings('defaultEdgeActiveColor') :
          edge.color,
        prefix = settings('prefix') || '',
        edgeColor = settings('edgeColor'),
        defaultNodeColor = settings('defaultNodeColor'),
        defaultEdgeColor = settings('defaultEdgeColor'),
        level = settings('edgeHoverLevel'),
        cp = {},
        size = settings('edgeHoverSizeRatio') * (edge[prefix + 'size'] || 1),
        tSize = target[prefix + 'size'],
        sX = source[prefix + 'x'],
        sY = source[prefix + 'y'],
        tX = target[prefix + 'x'],
        tY = target[prefix + 'y'],
        d,
        aSize,
        aX,
        aY,
        vX,
        vY;

    cp = (source.id === target.id) ?
      sigma.utils.getSelfLoopControlPoints(sX, sY, tSize) :
      sigma.utils.getQuadraticControlPoint(sX, sY, tX, tY, edge.cc);

    if (source.id === target.id) {
      d = Math.sqrt(Math.pow(tX - cp.x1, 2) + Math.pow(tY - cp.y1, 2));
      aSize = size * 2.5;
      aX = cp.x1 + (tX - cp.x1) * (d - aSize - tSize) / d;
      aY = cp.y1 + (tY - cp.y1) * (d - aSize - tSize) / d;
      vX = (tX - cp.x1) * aSize / d;
      vY = (tY - cp.y1) * aSize / d;
    }
    else {
      d = Math.sqrt(Math.pow(tX - cp.x, 2) + Math.pow(tY - cp.y, 2));
      aSize = Math.max(size * 2.5, settings('minArrowSize'));
      aX = cp.x + (tX - cp.x) * (d - aSize - tSize) / d;
      aY = cp.y + (tY - cp.y) * (d - aSize - tSize) / d;
      vX = (tX - cp.x) * aSize / d;
      vY = (tY - cp.y) * aSize / d;
    }

    // Level:
    if (level) {
      context.shadowOffsetX = 0;
      // inspired by Material Design shadows, level from 1 to 5:
      switch(level) {
        case 1:
          context.shadowOffsetY = 1.5;
          context.shadowBlur = 4;
          context.shadowColor = 'rgba(0,0,0,0.36)';
          break;
        case 2:
          context.shadowOffsetY = 3;
          context.shadowBlur = 12;
          context.shadowColor = 'rgba(0,0,0,0.39)';
          break;
        case 3:
          context.shadowOffsetY = 6;
          context.shadowBlur = 12;
          context.shadowColor = 'rgba(0,0,0,0.42)';
          break;
        case 4:
          context.shadowOffsetY = 10;
          context.shadowBlur = 20;
          context.shadowColor = 'rgba(0,0,0,0.47)';
          break;
        case 5:
          context.shadowOffsetY = 15;
          context.shadowBlur = 24;
          context.shadowColor = 'rgba(0,0,0,0.52)';
          break;
      }
    }

    if (!color)
      switch (edgeColor) {
        case 'source':
          color = source.color || defaultNodeColor;
          break;
        case 'target':
          color = target.color || defaultNodeColor;
          break;
        default:
          color = defaultEdgeColor;
          break;
      }

    if (settings('edgeHoverColor') === 'edge') {
      color = edge.hover_color || color;
    } else {
      color = edge.hover_color || settings('defaultEdgeHoverColor') || color;
    }

    context.strokeStyle = color;
    context.lineWidth = size;
    context.beginPath();
    context.moveTo(sX, sY);
    if (source.id === target.id) {
      context.bezierCurveTo(cp.x2, cp.y2, cp.x1, cp.y1, aX, aY);
    } else {
      context.quadraticCurveTo(cp.x, cp.y, aX, aY);
    }
    context.stroke();

    context.fillStyle = color;
    context.beginPath();
    context.moveTo(aX + vX, aY + vY);
    context.lineTo(aX + vY * 0.6, aY - vX * 0.6);
    context.lineTo(aX - vY * 0.6, aY + vX * 0.6);
    context.lineTo(aX + vX, aY + vY);
    context.closePath();
    context.fill();

    // reset shadow
    if (level) {
      context.shadowOffsetY = 0;
      context.shadowBlur = 0;
      context.shadowColor = '#000000'
    }

    // draw label with a background
    if (sigma.canvas.edges.labels && sigma.canvas.edges.labels.curvedArrow) {
      edge.hover = true;
      var def = sigma.canvas.edges.labels.curvedArrow;
      (def.render || def)(edge, source, target, context, settings);
      edge.hover = false;
    }
  };
})();
