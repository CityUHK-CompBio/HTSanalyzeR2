;(function(undefined) {
  'use strict';

  if (typeof sigma === 'undefined')
    throw 'sigma is not declared';

  // Initialize packages:
  sigma.utils.pkg('sigma.misc');

  /**
   * This helper will bind any no-DOM renderer (for instance canvas or WebGL)
   * to its captors, to properly dispatch the good events to the sigma instance
   * to manage clicking, hovering etc...
   *
   * It has to be called in the scope of the related renderer.
   */
  sigma.misc.bindEvents = function(prefix) {
    var i,
        l,
        mX,
        mY,
        captor,
        self = this;

    function getNodes(e) {
      if (e) {
        mX = 'x' in e.data ? e.data.x : mX;
        mY = 'y' in e.data ? e.data.y : mY;
      }

      var i,
          j,
          l,
          n,
          x,
          y,
          s,
          inserted,
          selected = [],
          modifiedX = mX + self.width / 2,
          modifiedY = mY + self.height / 2,
          point = self.camera.cameraPosition(
            mX,
            mY
          ),
          nodes = self.camera.quadtree.point(
            point.x,
            point.y
          );

      if (nodes.length)
        for (i = 0, l = nodes.length; i < l; i++) {
          n = nodes[i];
          x = n[prefix + 'x'];
          y = n[prefix + 'y'];
          s = n[prefix + 'size'];

          if (
            !n.hidden &&
            modifiedX > x - s &&
            modifiedX < x + s &&
            modifiedY > y - s &&
            modifiedY < y + s &&
            Math.sqrt(
              (modifiedX - x) * (modifiedX - x) +
              (modifiedY - y) * (modifiedY - y)
            ) < s
          ) {
            // Insert the node:
            inserted = false;

            for (j = 0; j < selected.length; j++)
              if (n.size > selected[j].size) {
                selected.splice(j, 0, n);
                inserted = true;
                break;
              }

            if (!inserted)
              selected.push(n);
          }
        }

      return selected;
    }


    function getEdges(e) {
      if (!self.settings('enableEdgeHovering')) {
        // No event if the setting is off:
        return [];
      }

      var isCanvas = (
        sigma.renderers.canvas && self instanceof sigma.renderers.canvas);

      if (!isCanvas) {
        // A quick hardcoded rule to prevent people from using this feature
        // with the WebGL renderer (which is not good enough at the moment):
        throw new Error(
          'The edge events feature is not compatible with the WebGL renderer'
        );
      }

      if (e) {
        mX = 'x' in e.data ? e.data.x : mX;
        mY = 'y' in e.data ? e.data.y : mY;
      }

      var i,
          j,
          l,
          a,
          edge,
          s,
          maxEpsilon = self.settings('edgeHoverPrecision'),
          source,
          target,
          cp,
          nodeIndex = {},
          inserted,
          selected = [],
          modifiedX = mX + self.width / 2,
          modifiedY = mY + self.height / 2,
          point = self.camera.cameraPosition(
            mX,
            mY
          ),
          edges = [];

      if (isCanvas) {
        var nodesOnScreen = self.camera.quadtree.area(
          self.camera.getRectangle(self.width, self.height)
        );
        for (a = nodesOnScreen, i = 0, l = a.length; i < l; i++)
          nodeIndex[a[i].id] = a[i];
      }

      if (self.camera.edgequadtree !== undefined) {
        edges = self.camera.edgequadtree.point(
          point.x,
          point.y
        );
      }

      function insertEdge(selected, edge) {
        inserted = false;

        for (j = 0; j < selected.length; j++)
          if (edge.size > selected[j].size) {
            selected.splice(j, 0, edge);
            inserted = true;
            break;
          }

        if (!inserted)
          selected.push(edge);
      }

      if (edges.length)
        for (i = 0, l = edges.length; i < l; i++) {
          edge = edges[i];
          source = self.graph.nodes(edge.source);
          target = self.graph.nodes(edge.target);
          // (HACK) we can't get edge[prefix + 'size'] on WebGL renderer:
          s = edge[prefix + 'size'] ||
              edge['read_' + prefix + 'size'];

          // First, let's identify which edges are drawn. To do this, we keep
          // every edges that have at least one extremity displayed according to
          // the quadtree and the "hidden" attribute. We also do not keep hidden
          // edges.
          // Then, let's check if the mouse is on the edge (we suppose that it
          // is a line segment).

          if (
            !edge.hidden &&
            !source.hidden && !target.hidden &&
            (!isCanvas ||
              (nodeIndex[edge.source] || nodeIndex[edge.target])) &&
            sigma.utils.getDistance(
              source[prefix + 'x'],
              source[prefix + 'y'],
              modifiedX,
              modifiedY) > source[prefix + 'size'] &&
            sigma.utils.getDistance(
              target[prefix + 'x'],
              target[prefix + 'y'],
              modifiedX,
              modifiedY) > target[prefix + 'size']
          ) {
            if (edge.type == 'curve' || edge.type == 'curvedArrow') {
              if (source.id === target.id) {
                cp = sigma.utils.getSelfLoopControlPoints(
                  source[prefix + 'x'],
                  source[prefix + 'y'],
                  source[prefix + 'size']
                );
                if (
                  sigma.utils.isPointOnBezierCurve(
                  modifiedX,
                  modifiedY,
                  source[prefix + 'x'],
                  source[prefix + 'y'],
                  target[prefix + 'x'],
                  target[prefix + 'y'],
                  cp.x1,
                  cp.y1,
                  cp.x2,
                  cp.y2,
                  Math.max(s, maxEpsilon)
                )) {
                  insertEdge(selected, edge);
                }
              }
              else {
                cp = sigma.utils.getQuadraticControlPoint(
                  source[prefix + 'x'],
                  source[prefix + 'y'],
                  target[prefix + 'x'],
                  target[prefix + 'y'],
                  edge.cc);
                if (
                  sigma.utils.isPointOnQuadraticCurve(
                  modifiedX,
                  modifiedY,
                  source[prefix + 'x'],
                  source[prefix + 'y'],
                  target[prefix + 'x'],
                  target[prefix + 'y'],
                  cp.x,
                  cp.y,
                  Math.max(s, maxEpsilon)
                )) {
                  insertEdge(selected, edge);
                }
              }
            } else if (
                sigma.utils.isPointOnSegment(
                modifiedX,
                modifiedY,
                source[prefix + 'x'],
                source[prefix + 'y'],
                target[prefix + 'x'],
                target[prefix + 'y'],
                Math.max(s, maxEpsilon)
              )) {
              insertEdge(selected, edge);
            }
          }
        }

      return selected;
    }


    function bindCaptor(captor) {
      var nodes,
          edges,
          overNodes = {},
          overEdges = {};

      function onClick(e) {
        if (!self.settings('eventsEnabled'))
          return;

        self.dispatchEvent('click', e.data);

        nodes = getNodes(e);
        edges = getEdges(e);

        if (nodes.length) {
          self.dispatchEvent('clickNode', {
            node: nodes[0],
            captor: e.data
          });
          self.dispatchEvent('clickNodes', {
            node: nodes,
            captor: e.data
          });
        } else if (edges.length) {
          self.dispatchEvent('clickEdge', {
            edge: edges[0],
            captor: e.data
          });
          self.dispatchEvent('clickEdges', {
            edge: edges,
            captor: e.data
          });
        } else
          self.dispatchEvent('clickStage', {captor: e.data});
      }

      function onDoubleClick(e) {
        if (!self.settings('eventsEnabled'))
          return;

        self.dispatchEvent('doubleClick', e.data);

        nodes = getNodes(e);
        edges = getEdges(e);

        if (nodes.length) {
          self.dispatchEvent('doubleClickNode', {
            node: nodes[0],
            captor: e.data
          });
          self.dispatchEvent('doubleClickNodes', {
            node: nodes,
            captor: e.data
          });
        } else if (edges.length) {
          self.dispatchEvent('doubleClickEdge', {
            edge: edges[0],
            captor: e.data
          });
          self.dispatchEvent('doubleClickEdges', {
            edge: edges,
            captor: e.data
          });
        } else
          self.dispatchEvent('doubleClickStage', {captor: e.data});
      }

      function onRightClick(e) {
        if (!self.settings('eventsEnabled'))
          return;

        self.dispatchEvent('rightClick', e.data);

        nodes = getNodes(e);
        edges = getEdges(e);

        if (nodes.length) {
          self.dispatchEvent('rightClickNode', {
            node: nodes[0],
            captor: e.data
          });
          self.dispatchEvent('rightClickNodes', {
            node: nodes,
            captor: e.data
          });
        } else if (edges.length) {
          self.dispatchEvent('rightClickEdge', {
            edge: edges[0],
            captor: e.data
          });
          self.dispatchEvent('rightClickEdges', {
            edge: edges,
            captor: e.data
          });
        } else
          self.dispatchEvent('rightClickStage', {captor: e.data});
      }

      function onOut(e) {
        if (!self.settings('eventsEnabled'))
          return;

        var k,
            event = {
              current: { nodes: [], edges: [], },
              enter: { nodes: [], edges: [], },
              leave: { nodes: [], edges: [], },
              captor: e.data
            },
            leave = event.leave;

        for (k in overNodes)
          leave.nodes.push(overNodes[k]);

        for (k in overEdges)
          leave.edges.push(overEdges[k]);

        overNodes = {};
        overEdges = {};

        if (leave.nodes.length || leave.edges.length) {
          self.dispatchEvent('hovers', event);
        }
      }

      function onMove(e) {
        if (!self.settings('eventsEnabled'))
          return;

        nodes = getNodes(e);
        edges = getEdges(e);

        var i,
            k,
            node,
            edge,
            newOutNodes = [],
            newOverNodes = [],
            currentOverNodes = {},
            newOutEdges = [],
            newOverEdges = [],
            currentOverEdges = {};

        // Check newly overred nodes:
        for (i = 0; i < nodes.length; i++) {
          node = nodes[i];
          currentOverNodes[node.id] = node;
          if (!overNodes[node.id]) {
            newOverNodes.push(node);
            overNodes[node.id] = node;
          }
        }

        // Check no more overred nodes:
        for (k in overNodes)
          if (!currentOverNodes[k]) {
            newOutNodes.push(overNodes[k]);
            delete overNodes[k];
          }

        // Check newly overred edges:
        for (i = 0; i < edges.length; i++) {
          edge = edges[i];
          currentOverEdges[edge.id] = edge;
          if (!overEdges[edge.id]) {
            newOverEdges.push(edge);
            overEdges[edge.id] = edge;
          }
        }

        // Check no more overred edges:
        for (k in overEdges)
          if (!currentOverEdges[k]) {
            newOutEdges.push(overEdges[k]);
            delete overEdges[k];
          }

        if (newOutEdges.length || newOverEdges.length ||
             newOutNodes.length || newOverNodes.length) {
          self.dispatchEvent('hovers', {
            current: {
              nodes: nodes,
              edges: edges,
            },
            enter: {
              nodes: newOverNodes,
              edges: newOverEdges,
            },
            leave: {
              nodes: newOutNodes,
              edges: newOutEdges,
            },
            captor: e.data
          });
        }
      }

      // Bind events:
      captor.bind('click', onClick);
      captor.bind('mousedown', onMove);
      captor.bind('mouseup', onMove);
      captor.bind('mousemove', onMove);
      captor.bind('mouseout', onOut);
      captor.bind('doubleclick', onDoubleClick);
      captor.bind('rightclick', onRightClick);
    }

    for (i = 0, l = this.captors.length; i < l; i++)
      bindCaptor(this.captors[i]);
  };
}).call(this);
