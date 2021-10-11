(function () {
    var ANIMATION_SPEED, BORDER_INSET, CELL_MIN, CELL_PAD, DEBUG_FORCE_FACTOR, EXPORT_PAD, GEOMETRY_VERSION, LABEL_SPACE, LINK_DISTANCE, LINK_STRENGTH, LoadingOverlay, MARGIN, MAX_ZOOM, MIN_ZOOM, NewNodesAnimation, ROUND_CORNER, SRC_PREVIEW_LIMIT, actionBlockSvg, actionSvg, applySCXMLLayout, envelope, findTransition, force, idMaker, idPath, scxmlLayout, midpoint, nextId, parents, path, strip, toSCXMLFormat, treeFromXml, walk;

    force = window.forceLayout = {};

    MARGIN = 5;

    ROUND_CORNER = 5;

    CELL_MIN = {
        w: 15,
        h: 15
    };

    CELL_PAD = {
        top: 12,
        bottom: 12,
        left: 12,
        right: 12
    };

    EXPORT_PAD = {
        top: 10,
        bottom: 10,
        left: 10,
        right: 10
    };

    LABEL_SPACE = 400;

    LINK_STRENGTH = .1;

    LINK_DISTANCE = 30;

    DEBUG_FORCE_FACTOR = 50;

    MIN_ZOOM = 1 / 6;

    MAX_ZOOM = 6;

    ANIMATION_SPEED = 2;

    GEOMETRY_VERSION = 2;

    BORDER_INSET = 3;

    SRC_PREVIEW_LIMIT = 40;

    strip = function (obj) {
        var key, value;
        for (key in obj) {
            value = obj[key];
            if (value !== null) {
                if (_.isArray(value) && value.length === 0) {
                    delete obj[key];
                } else if (_.isObject(value)) {
                    strip(value);
                    if (_.isEmpty(value)) {
                        delete obj[key];
                    }
                }
            } else {
                delete obj[key];
            }
        }
        return obj;
    };

    treeFromXml = function (doc, currentState) {
        var parseActions, parseChildNodes, parseStates;
        parseActions = function (container) {
            var action, child, firstLine, rv, _i, _len, _ref;
            rv = [];
            _ref = container.childNodes;
            for (_i = 0, _len = _ref.length; _i < _len; _i++) {
                child = _ref[_i];
                if (child.tagName) {
                    var tooltip = "";
                    var title = "";
                    var tagName = child.tagName;
                    if (child.tagName.indexOf(":") > 0) {
                        tagName = child.tagName.substring(child.tagName.indexOf(":") + 1);
                        title = child.tagName;
                        for (var att, i = 0, atts = child.attributes, n = atts.length; i < n; i++) {
                            att = atts[i];
                            tooltip += " <b>" + att.nodeName + ": </b>" + att.nodeValue + "<br />";
                        }
                    } else {
                        tagName = child.tagName.substring(child.tagName);
                        title = child.tagName;
                        for (var att, i = 0, atts = child.attributes, n = atts.length; i < n; i++) {
                            att = atts[i];
                            tooltip += " <b>" + att.nodeName + ": </b>" + att.nodeValue + "<br />";
                        }
                    }
                    rv.push(action = {
                        label: tagName,
                        title: title,
                        tooltip: tooltip
                    });
                    if (child.tagName === 'script') {
                        firstLine = $(child).text().trim().split(/\n/)[0];
                        if (firstLine.length > SRC_PREVIEW_LIMIT) {
                            firstLine = firstLine.slice(0, SRC_PREVIEW_LIMIT - 4) + ' ...';
                        }
                        action.preview = firstLine;
                    }
                }
            }
            return rv;
        };
        parseChildNodes = function (node) {
            var child, onentry, onexit, target, transitions, _i, _len, _ref;
            transitions = [];
            onentry = [];
            onexit = [];
            _ref = node.childNodes;
            for (_i = 0, _len = _ref.length; _i < _len; _i++) {
                child = _ref[_i];
                switch (child.tagName) {
                    case 'transition':
                        target = child.getAttribute('target');
                        if (target && target.indexOf(' ') > -1) {
                            throw new Error("not implemented: transition with multiple targets");
                        }
                        if (!target) {
                            target = node.getAttribute('id');
                        }
                        transitions.push(strip({
                            target: target,
                            cond: child.getAttribute('cond') || null,
                            event: child.getAttribute('event') || null,
                            actions: parseActions(child)
                        }));
                        break;
                    case 'onentry':
                        onentry = onentry.concat(parseActions(child));
                        break;
                    case 'onexit':
                        onexit = onexit.concat(parseActions(child));
                }
            }
            return {
                transitions: transitions,
                onentry: onentry,
                onexit: onexit
            };
        };
        parseStates = function (node, currentState) {
            var state, stateList, _i, _len, _ref, initialState;
            stateList = [];
            _ref = node.childNodes;
            if (node.tagName === 'scxml') {
                initialState = node.getAttribute('initial');
            }
            for (_i = 0, _len = _ref.length; _i < _len; _i++) {
                node = _ref[_i];
                state = (function () {
                    switch (node.tagName) {
                        case 'initial':
                            return {
                                type: 'initial' + ((currentState === node.getAttribute('id')) ? ' currentState' : ''),
                                id: node.getAttribute('id') || null,
                                children: parseStates(node, currentState),
                                currentState: currentState === node.getAttribute('id')
                            };
                        case 'state':
                            return {
                                type: ((initialState === node.getAttribute('id')) ? 'initial' : 'state') + ((currentState === node.getAttribute('id')) ? ' currentState' : ''),
                                id: node.getAttribute('id') || null,
                                children: parseStates(node, currentState),
                                currentState: currentState === node.getAttribute('id')
                            };
                        case 'final':
                            return {
                                type: 'final' + ((currentState === node.getAttribute('id')) ? ' currentState' : ''),
                                id: node.getAttribute('id') || null,
                                children: parseStates(node, currentState),
                                currentState: currentState === node.getAttribute('id')
                            };
                        case 'parallel':
                            return {
                                type: 'parallel' + ((currentState === node.getAttribute('id')) ? ' currentState' : ''),
                                id: node.getAttribute('id') || null,
                                children: parseStates(node, currentState),
                                currentState: currentState === node.getAttribute('id')
                            };
                        case 'history':
                            return {
                                type: 'history',
                                id: node.getAttribute('id') || null,
                                deep: node.getAttribute('type') === 'deep' || null
                            };
                    }
                })();
                if (state != null) {
                    _.extend(state, parseChildNodes(node, currentState));
                    stateList.push(strip(state));
                }
            }
            return stateList;
        };
        return {
            sc: parseStates(doc.documentElement, currentState)
        };
    };

    idMaker = function () {
        var counterMap;
        counterMap = d3.map();
        return function (prefix) {
            var counter;
            if (prefix == null) {
                prefix = '_force_id_';
            }
            counter = counterMap.get(prefix) || 0;
            counter += 1;
            counterMap.set(prefix, counter);
            return "" + prefix + counter;
        };
    };

    nextId = idMaker();

    walk = function (node, callback, parent, postorder) {
        var child, _i, _len, _ref;
        if (parent == null) {
            parent = null;
        }
        if (postorder == null) {
            postorder = false;
        }
        if (!postorder) {
            callback(node, parent);
        }
        _ref = node.children || [];
        for (_i = 0, _len = _ref.length; _i < _len; _i++) {
            child = _ref[_i];
            walk(child, callback, node, postorder);
        }
        if (postorder) {
            return callback(node, parent);
        }
    };

    parents = function (node) {
        if (node.parent) {
            return parents(node.parent).concat([node.parent]);
        } else {
            return [];
        }
    };

    idPath = function (node) {
        return parents(node).join('/');
    };

    path = function (node1, node2) {
        var eq, n, parents1, parents2, _i, _ref;
        parents1 = parents(node1);
        parents2 = parents(node2);
        eq = 0;
        for (n = _i = 0, _ref = d3.min([parents1.length, parents2.length]) - 1; 0 <= _ref ? _i <= _ref : _i >= _ref; n = 0 <= _ref ? ++_i : --_i) {
            if (parents1[n] !== parents2[n]) {
                break;
            }
            eq = n;
        }
        return [node1, parents1[eq], node2];
    };

    midpoint = function (a, b) {
        return {
            x: ((a.x || 0) + (b.x || 0)) / 2,
            y: ((a.y || 0) + (b.y || 0)) / 2
        };
    };

    findTransition = function (transitions, source, target) {
        var tr, _i, _len;
        for (_i = 0, _len = transitions.length; _i < _len; _i++) {
            tr = transitions[_i];
            if (tr.a.id === source && tr.b.id === target) {
                return tr;
            }
        }
    };

    envelope = function (node, pad) {
        var box, point, tr, xValues, yValues, _i, _j, _k, _len, _len1, _len2, _ref, _ref1, _ref2;
        if (pad == null) {
            pad = {};
        }
        xValues = [];
        yValues = [];
        _ref = [].concat(node.children, node.controls);
        for (_i = 0, _len = _ref.length; _i < _len; _i++) {
            box = _ref[_i];
            xValues.push(box.x - box.w / 2);
            xValues.push(box.x + box.w / 2);
            yValues.push(box.y - box.h / 2);
            yValues.push(box.y + box.h / 2);
        }
        _ref1 = node.controls;
        for (_j = 0, _len1 = _ref1.length; _j < _len1; _j++) {
            tr = _ref1[_j];
            _ref2 = [].concat(tr.route.segment1, tr.route.segment2);
            for (_k = 0, _len2 = _ref2.length; _k < _len2; _k++) {
                point = _ref2[_k];
                xValues.push(point[0]);
                yValues.push(point[1]);
            }
        }
        return [d3.min(xValues) - (pad.left || 0), d3.max(xValues) + (pad.right || 0), d3.min(yValues) - (pad.top || 0) - (node.topPadding || 0), d3.max(yValues) + (pad.bottom || 0)];
    };




    actionSvg = function (options) {
        var actionR, actionT, h, w;
        actionR = options.g.append('rect');
        actionT = options.g.append('text').attr('y', 17);
        if (options.action.tooltip !== undefined && options.action.tooltip !== '') {
            options.g.attr("title", function (v) {
                return styleTooltip(options.action.title, options.action.tooltip)
            }).each(function (v) {
                $(this).tipsy({ gravity: "s", opacity: 1, html: true });
            });
        }
        actionT
            .append('tspan')
            .text(options.action.label);
        if (options.action.preview) {
            actionT.append('tspan').attr('x', 0).attr('dy', 16).text(options.action.preview);
        }
        var h = $(actionT[0][0]).height() + 10;
        var w = $(actionT[0][0]).width() + 10;
        if (h === 10) {
            h = (actionT[0][0]).getBBox().height + 10;
            w = (actionT[0][0]).getBBox().width + 10;
        }
        actionR.attr('height', h).attr('width', w).attr('x', -w / 2).attr('rx', 5).attr('ry', 5);
        return [w, h];
    };

    actionBlockSvg = function (actions, parentG) {
        var action, actionG, h, maxw, w, y, _i, _len, _ref;
        y = 0;
        maxw = 0;
        for (_i = 0, _len = actions.length; _i < _len; _i++) {
            action = actions[_i];
            actionG = parentG.append('g').attr('class', 'action').attr('transform', "translate(0," + y + ")");
            _ref = actionSvg({
                action: action,
                g: actionG
            }), w = _ref[0], h = _ref[1];
            y += h;
            maxw = d3.max([maxw, w]);
        }
        return [maxw, y];
    };

    toSCXMLFormat = function (node) {
        var child, children, edges, node_header, node_min_size, rv, tr, _i, _j, _len, _len1, _ref, _ref1;
        children = [];
        edges = [];
        _ref = node.children || [];
        for (_i = 0, _len = _ref.length; _i < _len; _i++) {
            child = _ref[_i];
            children.push(toSCXMLFormat(child));
        }
        _ref1 = node.controls || [];
        for (_j = 0, _len1 = _ref1.length; _j < _len1; _j++) {
            tr = _ref1[_j];
            children.push({
                id: tr.id,
                desmTransition: true,
                width: tr.w,
                height: tr.h,
                ports: [
                    {
                        id: "" + tr.id + "#enter",
                        x: 0,
                        y: tr.yPort
                    }, {
                        id: "" + tr.id + "#exit",
                        x: tr.w,
                        y: tr.yPort
                    }
                ],
                properties: {
                    portConstraints: 'FIXED_POS'
                }
            });
            edges.push({
                id: "" + tr.id + "#1",
                source: tr.a.id,
                target: tr.id,
                targetPort: "" + tr.id + "#enter"
            });
            edges.push({
                id: "" + tr.id + "#2",
                source: tr.id,
                target: tr.b.id,
                sourcePort: "" + tr.id + "#exit"
            });
        }
        node_header = node.header || CELL_MIN;
        node_min_size = node.minSize || {
            w: 0,
            h: 0
        };
        rv = {
            id: node.id,
            children: children,
            edges: edges,
            padding: {
                top: node_header.h || 0
            },
            width: node_min_size.w,
            height: node_min_size.h,
            properties: {
                minWidth: node_min_size.w,
                minHeight: node_min_size.h,
                sizeConstraint: 'MINIMUM_SIZE'
            }
        };
        return rv;
    };

    applySCXMLLayout = function (options) {
        var graph, kEdgeMap, kNodeMap, offsetMap, s, traverse;
        s = options.s;
        graph = options.graph;
        kNodeMap = d3.map();
        kEdgeMap = d3.map();
        offsetMap = d3.map();
        offsetMap.set('__ROOT__', {
            x: -graph.width / 2,
            y: -graph.height / 2
        });
        walk(graph, (function (_this) {
            return function (kNode) {
                var kChild, kEdge, offset, padding, _i, _j, _len, _len1, _ref, _ref1, _results;
                kNodeMap.set(kNode.id, kNode);
                offset = offsetMap.get(kNode.id);
                padding = _.extend({
                    top: 0,
                    left: 0
                }, kNode.padding);
                _ref = kNode.children || [];
                for (_i = 0, _len = _ref.length; _i < _len; _i++) {
                    kChild = _ref[_i];
                    offsetMap.set(kChild.id, {
                        x: offset.x + (kNode.x || 0) + (padding.left || 0),
                        y: offset.y + (kNode.y || 0) + (padding.top || 0)
                    });
                }
                _ref1 = kNode.edges || [];
                _results = [];
                for (_j = 0, _len1 = _ref1.length; _j < _len1; _j++) {
                    kEdge = _ref1[_j];
                    _results.push(kEdgeMap.set(kEdge.id, kEdge));
                }
                return _results;
            };
        })(this));
        traverse = function (kNode) {
            var e1, e2, kChild, kTr, node, offset, offset1, offset2, tr, translate1, translate2, _i, _len, _ref, _results;
            if (kNode.desmTransition) {
                tr = s.nodeMap.get(kNode.id);
                offset1 = offsetMap.get(tr.a.id);
                offset2 = offsetMap.get(tr.id);
                kTr = kNodeMap.get(tr.id);
                tr.x = offset2.x + kTr.x + kTr.width / 2;
                tr.y = offset2.y + kTr.y + kTr.height / 2;
                e1 = kEdgeMap.get("" + tr.id + "#1");
                e2 = kEdgeMap.get("" + tr.id + "#2");
                translate1 = function (d) {
                    return [offset1.x + d.x, offset1.y + d.y];
                };
                translate2 = function (d) {
                    return [offset2.x + d.x, offset2.y + d.y];
                };
                tr.route = {
                    src: translate1(e1.sourcePoint),
                    segment1: (e1.bendPoints || []).map(translate1),
                    label1: translate1(e1.targetPoint),
                    label2: translate2(e2.sourcePoint),
                    segment2: (e2.bendPoints || []).map(translate2),
                    dst: translate2(e2.targetPoint)
                };
            } else if (kNode.id !== '__ROOT__') {
                node = s.nodeMap.get(kNode.id);
                offset = offsetMap.get(kNode.id);
                node.w = kNode.width;
                node.h = kNode.height;
                node.x = offset.x + (kNode.x || 0) + node.w / 2;
                node.y = offset.y + (kNode.y || 0) + node.h / 2;
            }
            _ref = kNode.children || [];
            _results = [];
            for (_i = 0, _len = _ref.length; _i < _len; _i++) {
                kChild = _ref[_i];
                _results.push(traverse(kChild));
            }
            return _results;
        };
        return traverse(graph);
    };

    scxmlLayout = function (s, options) {
        var algorithm, form, graph, klay_ready, layoutDone, top;
        algorithm = options.algorithm || '__klayjs';
        top = s.top;
        graph = toSCXMLFormat(top);
        if (algorithm === '__klayjs') {
            klay_ready = Q.defer();
            $klay.layout({
                graph: graph,
                options: {
                    layoutHierarchy: true,
                    edgeRouting: 'ORTHOGONAL',
                    feedBackEdges: true
                },
                success: klay_ready.resolve,
                error: function (err) {
                    return klay_ready.reject(new Error(err.text));
                }
            });
            layoutDone = klay_ready.promise;
        } else {
            form = {
                graph: JSON.stringify(graph),
                config: JSON.stringify({
                    algorithm: algorithm,
                    edgeRouting: 'ORTHOGONAL',
                    feedBackEdges: true,
                    layoutHierarchy: true
                }),
                iFormat: 'org.json',
                oFormat: 'org.json'
            };
        }
        return layoutDone;
    };

    NewNodesAnimation = (function () {
        function NewNodesAnimation(newNodes) {
            var node, _i, _len, _ref;
            this.newNodes = newNodes;
            this.deferred = Q.defer();
            this.promise = this.deferred.promise;
            this.done = false;
            this.targetMap = d3.map();
            if (!(this.newNodes.length > 0)) {
                this.abort();
            }
            _ref = this.newNodes;
            for (_i = 0, _len = _ref.length; _i < _len; _i++) {
                node = _ref[_i];
                this.targetMap.set(node.id, {
                    w: node.w,
                    h: node.h
                });
                node.w = node.h = 5;
            }
        }

        NewNodesAnimation.prototype.tick = function () {
            var changed, node, target, _i, _len, _ref;
            if (this.done) {
                return;
            }
            changed = false;
            _ref = this.newNodes;
            for (_i = 0, _len = _ref.length; _i < _len; _i++) {
                node = _ref[_i];
                target = this.targetMap.get(node.id);
                if (node.w < target.w) {
                    node.w += ANIMATION_SPEED;
                    changed = true;
                }
                if (node.h < target.h) {
                    node.h += ANIMATION_SPEED;
                    changed = true;
                }
            }
            if (!changed) {
                return this.abort();
            }
        };

        NewNodesAnimation.prototype.abort = function () {
            this.done = true;
            return this.deferred.resolve();
        };

        return NewNodesAnimation;

    })();

    LoadingOverlay = (function () {
        function LoadingOverlay(options) {
            var h, w;
            w = $(options.svg).width();
            h = $(options.svg).height();
            this.el = d3.select(options.svg).append('g').attr('class', "loadingOverlay");
            this.el.append('rect').attr('width', w).attr('height', h);
            this.el.append('text').attr('x', w / 2).attr('y', h / 2).text(options.text);
        }

        LoadingOverlay.prototype.destroy = function () {
            return this.el.remove();
        };

        return LoadingOverlay;

    })();

    force.Layout = (function () {
        function Layout(options) {
            this.id = nextId();
            this.queue = async.queue((function (task, cb) {
                return task(cb);
            }), 1);
            this.options = options;
            this.debug = options.debug || false;
            this.svgCreate(options.parent);
            this.s = this._emptyState();
            this.animation = new NewNodesAnimation([]);
            this._initialTree(options.tree || treeFromXml(options.doc, (options.currentState || null)).sc);
        }

        Layout.prototype._initialTree = function (tree) {
            var deferred;
            deferred = Q.defer();
            this.initialized = deferred.promise;
            return this.queue.push((function (_this) {
                return function (cb) {
                    var e, loading;
                    try {
                        _this.loadTree(tree);
                        if (_this.options.geometry != null) {
                            _this.applyGeometry(_this.options.geometry);
                            _this.svgUpdate();
                            cb();
                            return deferred.resolve();
                        } else {
                            loading = new LoadingOverlay({
                                svg: _this.el,
                                text: "Loading SCXML layout ..."
                            });
                            return deferred.resolve(_this._scxmlLayout().then(function () {
                                loading.destroy();
                                return cb();
                            }));
                        }
                    } catch (_error) {
                        e = _error;
                        deferred.reject(e);
                        return cb();
                    }
                };
            })(this));
        };

        Layout.prototype._scxmlLayout = function (options) {
            if (options == null) {
                options = {};
            }
            return scxmlLayout(this.s, {
                algorithm: this.options.scxmlAlgorithm
            }).then((function (_this) {
                return function (graph) {
                    return applySCXMLLayout({
                        s: _this.s,
                        graph: graph
                    });
                };
            })(this)).then((function (_this) {
                return function () {
                    return _this.svgUpdate({
                        animate: options.animate
                    });
                };
            })(this));
        };

        Layout.prototype.update = function (doc, currentState) {
            var deferred;
            deferred = Q.defer();
            this.queue.push((function (_this) {
                return function (cb) {
                    return deferred.resolve(Q().then(function () {
                        _this.loadTree(treeFromXml(doc, currentState).sc);
                        return _this._scxmlLayout({
                            animate: true
                        });
                    })["finally"](function () {
                        return cb();
                    }));
                };
            })(this));
            return deferred.promise;
        };

        Layout.prototype._emptyState = function () {
            var s;
            s = {
                nodes: [],
                cells: [],
                nodeMap: d3.map(),
                links: [],
                transitions: [],
                top: {
                    id: '__ROOT__',
                    children: [],
                    controls: []
                },
                newNodes: [],
                dom: d3.map()
            };
            s.nodeMap.set(s.top.id, s.top);
            return s;
        };

        Layout.prototype.loadTree = function (tree) {
            this.mergeTree(tree);
            return this.svgNodes();
        };

        Layout.prototype.mergeTree = function (tree) {
            var makeId, newS, oldS, topNode, _i, _j, _len, _len1;
            oldS = this.s;
            newS = this._emptyState();
            newS.top.children = tree;
            makeId = idMaker();
            for (_i = 0, _len = tree.length; _i < _len; _i++) {
                topNode = tree[_i];
                walk(topNode, (function (_this) {
                    return function (node, parent) {
                        var oldNode;
                        if (node.id) {
                            node.label = node.id;
                            node.autoId = false;
                        } else {
                            node.id = makeId("_node_");
                            node.autoId = true;
                            node.label = node.type;
                        }
                        node.isInitial = false;
                        node.controls = [];
                        node.children = node.children || [];
                        if ((oldNode = oldS.nodeMap.get(node.id)) != null) {
                            node.x = oldNode.x;
                            node.y = oldNode.y;
                            node.header = oldNode.header;
                        } else {
                            if (parent != null) {
                                node.x = parent.x;
                                node.y = parent.y;
                            }
                            newS.newNodes.push(node);
                        }
                        newS.nodes.push(node);
                        newS.cells.push(node);
                        newS.nodeMap.set(node.id, node);
                        return node.parent = parent != null ? newS.nodeMap.get(parent.id) : newS.top;
                    };
                })(this));
            }
            for (_j = 0, _len1 = tree.length; _j < _len1; _j++) {
                topNode = tree[_j];
                walk(topNode, (function (_this) {
                    return function (node) {
                        var a, b, c, label, link_source, link_target, oldTr, target, tr, _k, _l, _len2, _len3, _ref, _ref1, _ref2, _ref3, _results;
                        _ref = node.transitions || [];
                        _results = [];
                        for (_k = 0, _len2 = _ref.length; _k < _len2; _k++) {
                            tr = _ref[_k];
                            if ((target = newS.nodeMap.get(tr.target)) == null) {
                                throw Error("missing transition target: " + tr.target);
                            }
                            _ref1 = path(node, target), a = _ref1[0], c = _ref1[1], b = _ref1[2];
                            tr.parent = c || newS.top;
                            tr.id = tr.id || makeId("_transition/" + node.id + "/" + target.id + "/");
                            if ((oldTr = oldS.nodeMap.get(tr.id)) != null) {
                                tr.w = oldTr.w;
                                tr.h = oldTr.h;
                                tr.yPort = oldTr.yPort;
                            } else {
                                tr.w = 0;
                                tr.h = 0;
                                tr.yPort = 0;
                            }
                            newS.nodeMap.set(tr.id, tr);
                            tr.parent.controls.push(tr);
                            newS.nodes.push(tr);
                            _ref2 = d3.pairs([a, tr, b]);
                            for (_l = 0, _len3 = _ref2.length; _l < _len3; _l++) {
                                _ref3 = _ref2[_l], link_source = _ref3[0], link_target = _ref3[1];
                                newS.links.push({
                                    source: link_source,
                                    target: link_target
                                });
                            }
                            label = tr.event || '';
                            tr.a = a;
                            tr.b = b;
                            tr.selfie = node.id === tr.target;
                            tr.label = label;
                            newS.transitions.push(tr);
                            if ((oldTr = findTransition(oldS.transitions, tr.a.id, tr.b.id)) != null) {
                                _results.push(_.extend(tr, {
                                    x: oldTr.x,
                                    y: oldTr.y
                                }));
                            } else {
                                _results.push(_.extend(tr, midpoint(tr.a, tr.b)));
                            }
                        }
                        return _results;
                    };
                })(this));
            }
            walk({
                children: tree
            }, (function (_this) {
                return function (node) {
                    //                    var child, first, _k, _len2, _ref;
                    if (!node.children.length) {
                        return;
                    }
                    //                    _ref = node.children;
                    //                    for (_k = 0, _len2 = _ref.length; _k < _len2; _k++) {
                    //                        child = _ref[_k];
                    //                        if (child.type === 'initial') {
                    //                            child.isInitial = true;
                    //                            return;
                    //                        }
                    //                        if (child.id === '@initial' && !child.children.length) {
                    //                            child.isInitial = true;
                    //                            return;
                    //                        }
                    //                    }
                    //                    first = node.children[0];
                    //                    if (first.autoId && first.children.length === 0) {
                    //                        return first.isInitial = true;
                    //                    }
                };
            })(this));
            return this.s = newS;
        };

        Layout.prototype.saveGeometry = function () {
            var n, round, tr;
            round = function (x) {
                return Math.round(x);
            };
            return JSON.stringify({
                nodes: (function () {
                    var _i, _len, _ref, _results;
                    _ref = this.s.nodes;
                    _results = [];
                    for (_i = 0, _len = _ref.length; _i < _len; _i++) {
                        n = _ref[_i];
                        _results.push({
                            id: n.id,
                            w: round(n.w),
                            h: round(n.h),
                            x: round(n.x),
                            y: round(n.y)
                        });
                    }
                    return _results;
                }).call(this),
                transitions: (function () {
                    var _i, _len, _ref, _results;
                    _ref = this.s.transitions;
                    _results = [];
                    for (_i = 0, _len = _ref.length; _i < _len; _i++) {
                        tr = _ref[_i];
                        _results.push({
                            id: tr.id,
                            route: tr.route
                        });
                    }
                    return _results;
                }).call(this),
                version: GEOMETRY_VERSION
            });
        };

        Layout.prototype.applyGeometry = function (geom_json) {
            var geom, node, saved, tr, _i, _j, _len, _len1, _ref, _ref1;
            geom = JSON.parse(geom_json);
            if (geom.version !== GEOMETRY_VERSION) {
                return;
            }
            _ref = geom.nodes;
            for (_i = 0, _len = _ref.length; _i < _len; _i++) {
                saved = _ref[_i];
                if ((node = this.s.nodeMap.get(saved.id)) != null) {
                    node.w = saved.w;
                    node.h = saved.h;
                }
            }
            _ref1 = geom.transitions || [];
            for (_j = 0, _len1 = _ref1.length; _j < _len1; _j++) {
                saved = _ref1[_j];
                if ((tr = this.s.nodeMap.get(saved.id)) != null) {
                    tr.route = saved.route;
                }
            }
            return this.svgUpdate();
        };

        Layout.prototype.svgCreate = function (parent) {
            var defs;
            this.zoomBehavior = d3.behavior.zoom().scaleExtent([MIN_ZOOM, MAX_ZOOM]);
            this.svg = d3.select(parent).append('svg').attr('xmlns:xmlns:xlink', 'http://www.w3.org/1999/xlink').classed('force-layout', true).classed('debug', this.debug);
            this.el = this.svg[0][0];
            defs = this.svg.append('defs');
            this.svg.call(this.zoomBehavior);
            this.container = this.svg.append('g');
            this.zoomBehavior.on('zoom', (function (_this) {
                return function () {
                    var e;
                    e = d3.event;
                    return _this.container.attr('transform', "translate(" + e.translate + "),scale(" + e.scale + ")");
                };
            })(this));
            defs.append('marker').attr('id', "" + this.id + "-arrow").attr('refX', '7').attr('refY', '5').attr('markerWidth', '10').attr('markerHeight', '10').attr('orient', 'auto').append('path').attr('d', 'M 0 0 L 10 5 L 0 10 z').attr('class', 'arrow');
            return this.invalidateSize();
        };

        Layout.prototype.invalidateSize = function () {
            var $parent, height, width;
            $parent = $(this.el).parent();
            width = $parent.width() - 5;
            height = $parent.height() - 5;
            d3.select(this.el).attr('width', width).attr('height', height);
            this.zoomBehavior.size([width, height]).translate([width / 2, height / 2]);
            return this.zoomBehavior.event(this.svg);
        };

        Layout.prototype.svgNodes = function () {
            var cellUpdate, dom, newCell, transitionG, transitionUpdate, titleCell, onEntryCell, onExitCell;
            cellUpdate = this.container.selectAll('.cell').data(this.s.cells, function (d) {
                return d.id;
            });
            newCell = cellUpdate.enter().append('g');
            titleCell = newCell.append('path').attr('class', 'title');
            onEntryCell = newCell.append('path').attr('class', 'onentry');
            onExitCell = newCell.append('path').attr('class', 'onexit');
            newCell.append('rect').attr('class', 'border');
            newCell.append('g').attr('class', 'cell-header');
            cellUpdate.each(function (node) {
                var corner_radius, h, hEntry, hExit, header, label, labelTextWidth, label_text, onentry, onexit, w, wEntry, wExit, wLabel, _ref, _ref1;
                d3.select(this).attr('class', "cell cell-" + (node.type || 'state') + " " + (node.isInitial ? 'cell-isInitial' : '')).classed('parallel-child', node.parent.type === 'parallel');
                header = d3.select(this).select('.cell-header');
                header.selectAll('*').remove();
                if (node.isInitial) {
                    node.minSize = {
                        w: 10,
                        h: 10
                    };
                    return;
                }
                if (node.type === 'final') {
                    d3.select(this).selectAll('.border-inset').remove();
                    d3.select(this).append('rect').attr('class', 'border-inset').attr('rx', ROUND_CORNER).attr('ry', ROUND_CORNER);
                }
                if (node.type === 'history') {
                    label_text = 'H';
                    corner_radius = 100;
                } else {
                    label_text = node.label;
                    corner_radius = ROUND_CORNER;
                }

                if (!node.type.startsWith('initial')) {
                    label = header.append('text').attr('class', "state-name").text(label_text).attr('y', 12);
                    labelTextWidth = $(label[0][0]).width();
                    if (labelTextWidth === 0) {
                        labelTextWidth = (label[0][0]).getBBox().width;
                    }
                    wLabel = d3.min([labelTextWidth + 2 * ROUND_CORNER, LABEL_SPACE]);
                } else {
                    wLabel = 14;
                    corner_radius = 14;
                }
                d3.select(this).select('.border').attr('rx', corner_radius).attr('ry', corner_radius);
                node.textWidth = wLabel;
                onentry = header.append('g');
                onexit = header.append('g');
                _ref = actionBlockSvg(node.onentry || [], onentry), wEntry = _ref[0], hEntry = _ref[1];
                _ref1 = actionBlockSvg(node.onexit || [], onexit), wExit = _ref1[0], hExit = _ref1[1];
                w = wEntry + wLabel + wExit;
                h = d3.max([hEntry, hExit]) + 24;
                if (node.type === 'history') {
                    h = w;
                }
                onentry.attr('transform', "translate(" + (wEntry / 2 - w / 2) + ",24)");
                onexit.attr('transform', "translate(" + (w / 2 - wExit / 2) + ",24)");
                node.header = {
                    w: w,
                    h: h
                };
                return node.minSize = {
                    w: w + 10,
                    h: h + 10
                };
            });
            cellUpdate.exit().remove();
            this.container.selectAll('.cell').sort(function (a, b) {
                return d3.ascending(idPath(a), idPath(b));
            });
            transitionUpdate = this.container.selectAll('.transition').data(this.s.transitions, function (d) {
                return d.id;
            });
            transitionG = transitionUpdate.enter().append('g').attr('class', 'transition');
            transitionG.append('path').attr('class', 'transitionMask');
            transitionG.append('path').attr('class', 'transitionLine').attr('style', "marker-end: url(#" + this.id + "-arrow)").attr('id', (function (_this) {
                return function (tr) {
                    return "" + _this.id + "-transition/" + tr.id;
                };
            })(this));
            transitionG.append('g').attr('class', 'transition-label').append('g').attr('class', 'transition-label-offset');
            transitionUpdate.exit().remove();
            transitionUpdate.each(function (tr) {
                var actionBlockG, h, offsetG, transitionRect, transitionText, w, y, _ref;
                offsetG = d3.select(this).select('.transition-label-offset');
                offsetG.selectAll('*').remove();
                transitionRect = offsetG.append('rect');
                transitionText = offsetG.append('text').attr('y', 16);
                transitionText.append('tspan').text(tr.label);
                if (tr.cond != null) {
                    transitionText.append('tspan').text("[" + tr.cond + "]").attr('x', 0).attr('dy', 16);
                    y += 16;
                }
                y = $(transitionText[0][0]).height() + 4;
                if (y === 4) {
                    y = (transitionText[0][0]).getBBox().height + 4;
                }
                tr.yPort = y - 2;
                actionBlockG = offsetG.append('g').attr('transform', "translate(0," + y + ")");
                _ref = actionBlockSvg(tr.actions || [], actionBlockG), w = _ref[0], h = _ref[1];
                y += h;
                var testWidth = $(transitionText[0][0]).width() + 5;
                if (testWidth === 5) {
                    testWidth = (transitionText[0][0]).getBBox().width + 5;
                }
                tr.textWidth = d3.min([testWidth, LABEL_SPACE]);
                tr.w = d3.max([tr.w, tr.textWidth, w]);
                tr.h = y + 4;
                offsetG.attr('transform', "translate(0," + (-tr.h / 2) + ")");
                return transitionRect.attr('x', function (tr) {
                    return -tr.w / 2;
                }).attr('width', function (tr) {
                    return tr.w;
                }).attr('height', function (tr) {
                    return tr.h;
                });
            });
            dom = this.s.dom;
            this.container.selectAll('.cell').each(function (node) {
                return dom.set("cell-" + node.id, this);
            });
            return this.container.selectAll('.transition').each(function (node) {
                return dom.set("transition-" + node.id, this);
            });
        };

        Layout.prototype.svgUpdate = function (options) {
            var animate, trPath;
            options = _.extend({
                animate: false
            }, options);
            if (options.animate) {
                animate = function (sel) {
                    return sel.transition();
                };
            } else {
                animate = function (sel) {
                    return sel;
                };
            }
            this.container.selectAll('.cell').classed('fixed', function (node) {
                return node.fixed;
            });
            animate(this.container.selectAll('.cell')).attr('transform', function (node) {
                return "translate(" + node.x + "," + node.y + ")";
            });
            this.container.selectAll('.cell').each(function (node) {
                var hasContent;
                console.log(node);
                if ((node.onentry !== undefined) && (node.onexit !== undefined)) {
                    animate(d3.select(this).select('.onentry')).attr('d', rounded_rect(-node.w / 2, 24 - (node.h / 2), node.w / 2, node.h - 24, 5, false, false, true, false));
                    animate(d3.select(this).select('.onexit')).attr('d', rounded_rect(0, 24 - (node.h / 2), node.w / 2, node.h - 24, 5, false, false, false, true));
                    hasContent = true;
                } else if ((node.onentry !== undefined) && (node.onexit === undefined)) {
                    animate(d3.select(this).select('.onentry')).attr('d', rounded_rect(-node.w / 2, 24 - (node.h / 2), node.w, node.h - 24, 5, false, false, true, true));
                    hasContent = true;
                } else if ((node.onentry === undefined) && (node.onexit !== undefined)) {
                    animate(d3.select(this).select('.onexit')).attr('d', rounded_rect(-node.w / 2, 24 - (node.h / 2), node.w, node.h - 24, 5, false, false, true, true));
                    hasContent = true;
                } else {
                    d3.select(this).select('.onentry').remove();
                    d3.select(this).select('.onexit').remove();
                    if (node.children.length > 0) {
                        hasContent = true;
                    } else {
                        hasContent = false;
                    }
                }
                if (hasContent) {
                    animate(d3.select(this).select('.border')).attr('x', -node.w / 2).attr('y', -node.h / 2).attr('width', node.w).attr('height', node.h);
                    animate(d3.select(this).select('.title')).attr('d', rounded_rect(-node.w / 2, -(node.h / 2), node.w, 24, 5, true, true, false, false));
                    animate(d3.select(this).select('.border-inset')).attr('x', -node.w / 2 + BORDER_INSET).attr('y', -node.h / 2 + BORDER_INSET).attr('width', node.w - 2 * BORDER_INSET).attr('height', node.h - 2 * BORDER_INSET);
                } else if (!node.type.startsWith('initial')) {
                    node.h = node.h - 10;
                    animate(d3.select(this).select('.border')).attr('x', -node.w / 2).attr('y', -node.h / 2).attr('width', node.w).attr('height', node.h);
                    animate(d3.select(this).select('.title')).attr('d', rounded_rect(-node.w / 2, -(node.h / 2), node.w, 24, 5, true, true, true, true));
                    animate(d3.select(this).select('.border-inset')).attr('x', -node.w / 2 + BORDER_INSET).attr('y', -node.h / 2 + BORDER_INSET).attr('width', node.w - 2 * BORDER_INSET).attr('height', node.h - 2 * BORDER_INSET);
                } else {
                    node.h = node.h - 10;
                    animate(d3.select(this).select('.border')).attr('x', -node.w / 2).attr('y', -node.h / 2).attr('width', node.w).attr('height', node.h);
                    animate(d3.select(this).select('.title')).attr('d', rounded_rect(-node.w / 2, -(node.h / 2), node.w, 24, 14, true, true, true, true));
                    animate(d3.select(this).select('.border-inset')).attr('x', -node.w / 2 + BORDER_INSET).attr('y', -node.h / 2 + BORDER_INSET).attr('width', node.w - 2 * BORDER_INSET).attr('height', node.h - 2 * BORDER_INSET);
                }
                return animate(d3.select(this).select('.cell-header')).attr('transform', function (node) {
                    return "translate(0," + (5 - node.h / 2) + ")";
                });
            });
            trPath = function (tr) {
                return d3.svg.line()([].concat([tr.route.src], tr.route.segment1, [tr.route.label1], [tr.route.label2], tr.route.segment2, [tr.route.dst]));
            };
            animate(this.container.selectAll('.transition').select('.transitionMask')).attr('d', trPath);
            animate(this.container.selectAll('.transition').select('.transitionLine')).attr('d', trPath);
            return animate(this.container.selectAll('.transition').select('.transition-label')).attr('transform', function (tr) {
                return "translate(" + tr.x + "," + tr.y + ")";
            });
        };

        Layout.prototype.moveNode = function (node, dx, dy) {
            var child, control, tr, translate, _i, _j, _k, _len, _len1, _len2, _ref, _ref1, _ref2, _results;
            node.x += dx;
            node.y += dy;
            translate = function (p, dx, dy) {
                p[0] += dx;
                return p[1] += dy;
            };
            if (node.route != null) {
                translate(node.route.label1, dx, dy);
                translate(node.route.label2, dx, dy);
            } else {
                _ref = this.s.transitions;
                for (_i = 0, _len = _ref.length; _i < _len; _i++) {
                    tr = _ref[_i];
                    if (tr.a.id === node.id) {
                        translate(tr.route.src, dx, dy);
                    }
                    if (tr.b.id === node.id) {
                        translate(tr.route.dst, dx, dy);
                    }
                }
            }
            _ref1 = node.children || [];
            for (_j = 0, _len1 = _ref1.length; _j < _len1; _j++) {
                child = _ref1[_j];
                this.moveNode(child, dx, dy);
            }
            _ref2 = node.controls || [];
            _results = [];
            for (_k = 0, _len2 = _ref2.length; _k < _len2; _k++) {
                control = _ref2[_k];
                _results.push(this.moveNode(control, dx, dy));
            }
            return _results;
        };

        Layout.prototype.adjustLayout = function () {
            var adjustNode, handleCollisions, node, _i, _len, _ref;
            handleCollisions = (function (_this) {
                return function (parent, center) {
                    var collide, node, nx1, nx2, ny1, ny2, objects, q, _i, _len, _results;
                    objects = [].concat(parent.children, parent.controls);
                    q = d3.geom.quadtree(objects);
                    _results = [];
                    for (_i = 0, _len = objects.length; _i < _len; _i++) {
                        node = objects[_i];
                        nx1 = node.x - node.w - 100;
                        nx2 = node.x + node.w + 100;
                        ny1 = node.y - node.h - 100;
                        ny2 = node.y + node.h + 100;
                        collide = function (quad, x1, y1, x2, y2) {
                            var cx, cy, dx, dx1, dx2, dy, dy1, dy2, f, h, na, oa, other, s, w;
                            other = quad.point;
                            if (other && (other !== node)) {
                                dx = node.x - other.x;
                                dy = node.y - other.y;
                                w = (node.w + other.w) / 2 + MARGIN;
                                h = (node.h + other.h) / 2 + MARGIN;
                                cx = w - Math.abs(dx);
                                cy = h - Math.abs(dy);
                                if (cx > 0 && cy > 0) {
                                    na = node.w * node.h;
                                    oa = other.w * other.h;
                                    f = oa / (oa + na);
                                    if (cx / w < cy / h) {
                                        dy1 = dy2 = 0;
                                        s = dx > 0 ? 1 : -1;
                                        dx1 = s * f * cx;
                                        dx2 = s * (f - 1) * cx;
                                    } else {
                                        dx1 = dx2 = 0;
                                        s = dy > 0 ? 1 : -1;
                                        dy1 = s * f * cy;
                                        dy2 = s * (f - 1) * cy;
                                    }
                                    _this.moveNode(node, dx1, dy1);
                                    _this.moveNode(other, dx2, dy2);
                                }
                            }
                            return x1 > nx2 || x2 < nx1 || y1 > ny2 || y2 < ny1;
                        };
                        _results.push(q.visit(collide));
                    }
                    return _results;
                };
            })(this);
            adjustNode = (function (_this) {
                return function (node) {
                    var dx, dy, grow, xMax, xMin, yMax, yMin, _ref;
                    if (node.children.length > 0) {
                        handleCollisions(node, node);
                        _ref = envelope(node, CELL_PAD), xMin = _ref[0], xMax = _ref[1], yMin = _ref[2], yMax = _ref[3];
                        grow = node.textWidth - (xMax - xMin);
                        if (grow > 0) {
                            xMin -= grow / 2;
                            xMax += grow / 2;
                        }
                        node.w = xMax - xMin;
                        node.h = yMax - yMin;
                        dx = xMin + node.w / 2 - node.x;
                        dy = yMin + node.h / 2 - node.y;
                        node.x += dx;
                        node.y += dy;
                        if (node.fixed) {
                            _this.moveNode(node, -dx, -dy);
                        }
                    }
                    return node.weight = node.w * node.h;
                };
            })(this);
            _ref = this.s.top.children;
            for (_i = 0, _len = _ref.length; _i < _len; _i++) {
                node = _ref[_i];
                walk(node, adjustNode, null, true);
            }
            return handleCollisions(this.s.top, {
                x: 0,
                y: 0
            });
        };

        Layout.prototype.highlightState = function (id, highlight) {
            if (highlight == null) {
                highlight = true;
            }
            return this.queue.push((function (_this) {
                return function (cb) {
                    d3.select(_this.s.dom.get("cell-" + id)).classed('highlight', highlight);
                    return cb();
                };
            })(this));
        };

        Layout.prototype.unhighlightAllStates = function () {
            return this.queue.push((function (_this) {
                return function (cb) {
                    d3.select(_this.el).selectAll('.cell.highlight').classed('highlight', false);
                    return cb();
                };
            })(this));
        };

        Layout.prototype.highlightTransition = function (source, target, highlight) {
            if (highlight == null) {
                highlight = true;
            }
            return this.queue.push((function (_this) {
                return function (cb) {
                    var tr;
                    if ((tr = findTransition(_this.s.transitions, source, target)) != null) {
                        d3.select(_this.s.dom.get("transition-" + tr.id)).classed('highlight', highlight);
                    }
                    return cb();
                };
            })(this));
        };

        Layout.prototype.fit = function () {
            return this.queue.push((function (_this) {
                return function (cb) {
                    var h, scale, w, xMax, xMin, yMax, yMin, _ref, _ref1;
                    _ref = envelope(_this.s.top, EXPORT_PAD), xMin = _ref[0], xMax = _ref[1], yMin = _ref[2], yMax = _ref[3];
                    _ref1 = _this.zoomBehavior.size(), w = _ref1[0], h = _ref1[1];
                    scale = d3.min([w / (xMax - xMin), h / (yMax - yMin)]);
                    _this.zoomBehavior.translate([w / 2 - (xMax + xMin) * scale / 2, h / 2 - (yMax + yMin) * scale / 2]);
                    _this.zoomBehavior.scale(scale);
                    _this.zoomBehavior.event(_this.svg);
                    return cb();
                };
            })(this));
        };

        Layout.prototype.exportSvg = function (options) {
            var bbox, container, defs, div, svg, xMax, xMin, yMax, yMin, _ref;
            _ref = envelope(this.s.top, EXPORT_PAD), xMin = _ref[0], xMax = _ref[1], yMin = _ref[2], yMax = _ref[3];
            div = $('<div style="positoin:relative">')[0];
            svg = d3.select(div).append('svg').attr('xmlns', 'http://www.w3.org/2000/svg').classed('force-layout', true);
            defs = d3.select(this.el).select('defs')[0][0].cloneNode(true);
            svg[0][0].appendChild(defs);
            d3.select(defs).append('style').text(options.css);
            container = this.container[0][0].cloneNode(true);
            d3.select(container).attr('transform', null);
            svg[0][0].appendChild(container);
            $('body').append(div);
            bbox = container.getBBox();
            $(div).remove();
            svg.attr('viewBox', "" + bbox.x + " " + bbox.y + " " + bbox.width + " " + bbox.height);
            return div.innerHTML;
        };

        return Layout;

    })();

    force.render = function (options) {
        return new force.Layout(options);
    };
}).call(this);

//# sourceMappingURL=forceLayout.js.map
// Simple function to style the tooltip for the given node.
var styleTooltip = function (name, description) {
    return "<p class='name'>" + name + "</p><p class='description'>" + description + "</p>";
};

function rounded_rect(x, y, w, h, r, tl, tr, bl, br) {
    var retval;
    retval = "M" + (x + r) + "," + y;
    retval += "h" + (w - 2 * r);
    if (tr) {
        retval += "a" + r + "," + r + " 0 0 1 " + r + "," + r;
    } else {
        retval += "h" + r;
        retval += "v" + r;
    }
    retval += "v" + (h - 2 * r);
    if (br) {
        retval += "a" + r + "," + r + " 0 0 1 " + -r + "," + r;
    } else {
        retval += "v" + r;
        retval += "h" + -r;
    }
    retval += "h" + (2 * r - w);
    if (bl) {
        retval += "a" + r + "," + r + " 0 0 1 " + -r + "," + -r;
    } else {
        retval += "h" + -r;
        retval += "v" + -r;
    }
    retval += "v" + (2 * r - h);
    if (tl) {
        retval += "a" + r + "," + r + " 0 0 1 " + r + "," + -r;
    } else {
        retval += "v" + -r;
        retval += "h" + r;
    }
    retval += "z";
    return retval;
}