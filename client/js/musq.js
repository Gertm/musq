var musq = function() {

    //##############################################################################################

    var utils = function() {

        function toPx(i) {
            return i + "px";
        }

        function fromPx(s) {
            return s.substr(0, s.length - 2);
        }

        function lerp(i1, i2, f) {
            return i1 + (i2 - i1) * f;
        }

        function onclickOffset(evt, axis, obj) {
            // [Randy 02/10/2010] REMARK: offsetX/Y isn't available in FireFox.
            var offsetAxis = evt["offset" + axis];
            if (offsetAxis) {
                return offsetAxis;
            }
            else if (axis == "X") {
                return evt.clientX - fromPx(obj.style.left);
            }
            else {
                return evt.clientY - fromPx(obj.style.top);
            }
        }

        function onkeyKey(evt) {
            var e = window.event || evt;
            var keyunicode = e.charCode || e.keyCode;            
            return keyunicode;
        }

        function removeTrailingEnter(s) {
            if (s == "") {
                return s;
            }
            if (s.charAt(s.length - 1) == '\n') {
                return s.substr(0, s.length - 1);
            }
            return s;
        }

        return {
            toPx: toPx,
            fromPx: fromPx,
            lerp: lerp,
            onclickOffset: onclickOffset,
            onkeyKey: onkeyKey,
            removeTrailingEnter: removeTrailingEnter
        };

    }();

    //##############################################################################################

    var vecMath = function() {

        function vector2d(x, y) {
            this.x = x;
            this.y = y;
            this.length = function() {
                return Math.sqrt(this.x * this.x + this.y * this.y);
            };
        }

        function add(v1, v2) {
            return new vector2d(v1.x + v2.x, v1.y + v2.y);
        }

        function subtract(v1, v2) {
            return new vector2d(v1.x - v2.x, v1.y - v2.y);
        }

        function scale(v, s) {
            return new vector2d(s * v.x, s * v.y);
        }

        function normalize(v) {
            return scale(v, 1.0 / v.length());
        };

        return {
            vector2d: vector2d,
            add: add,
            subtract: subtract,
            scale: scale,
            normalize: normalize
        };

    }();

    //##############################################################################################

    var data = {};
    // These are all in 'logical' coordinates, not in UI pixels.
    data.viewPortCenter = new vecMath.vector2d(0.0, 0.0);
    data.playerUiSide = new vecMath.vector2d(0.0, 0.0);
    data.playerLogicSide = new vecMath.vector2d(0.0, 0.0);
    data.now = function() {
        return (new Date()).getTime();
    };
    data.lastUpdateTime = data.now();
    data.talking = false;
    data.footerHeight = 20;
    data.logicalToVisualFactor = 70.0;

    //##############################################################################################

    function log(txt) {
        if (console.log)
            console.log("MUSQ: " + txt);
    }

    //##############################################################################################

    var communication = function() {

        if (window.WebSocket) {

            var ws = new WebSocket("ws://"+musq_websocket_url+"/service");

            function send(obj) {
                ws.send(JSON.stringify(obj));
            }

            function sendKeepAliv(obj) {
                send({
                         "Function": "keepalive",
                         "Params": {}
                     });
            }

            ws.onopen = function() {
                log("WebSocket opened.");
                send({
                         "Function": "login",
                         "Params": {
                             "Username": "Randy",
                             "Password": ""
                         }
                     });
                setInterval(sendKeepAliv, 10000);  
            };

            ws.onclose = function() {
                log("WebSocket closed.");
            };

            ws.onmessage = function(evt) {
                log("Received " + evt.data + ".");
                var json = JSON.parse(evt.data);
                if (!json) {
                    log("Unable to parse as JSON.");
                    return;
                }
                if (!json.Function) {
                    log("JSON has unexpected format.");
                    return;
                }
                if (json.Function == "login") {
                    return;
                }
                if (json.Function == "keepalive") {
                    return;
                }
                if (json.Function == "move") {
                    data.playerLogicSide = new vecMath.vector2d(parseInt(json.Params.X), parseInt(json.Params.Y));
                    return;
                }
                if (json.Function == "talk") {
                    alert(json.Params.Message);
                    return;
                }
            };

            return {
                send: send
            };

        } else {

            log("Sorry, your browser does not support websockets.");
            return {};

        }

    }();

    //##############################################################################################

    var main = function() {

        var resourceBuffer = function() {

            var container = {};

            function addImage(key, url) {
                var image = new Image();
                image.src = url;
                container[key] = image;
            }

            function addXml(key, url) {
                var xmlHttp = new XMLHttpRequest();
                xmlHttp.open("GET", url, false);
                xmlHttp.send();
                container[key] = xmlHttp.responseText;
            }

            function remove(key) {
                container[key] = undefined;
            }

            function get(key) {
                return container[key];
            }

            return {
                addImage: addImage,
                addXml: addXml,
                remove: remove,
                get: get
            };

        }();

        function positionCanvas() {
            var xPadding = 40;
            var yPadding = 20;
            data.canvas.style.position = "fixed";
            var newWidth = window.innerWidth - xPadding * 2;
            data.canvas.setAttribute("width", utils.toPx(newWidth));
            var newHeight = window.innerHeight - yPadding * 2 - data.footerHeight;
            data.canvas.setAttribute("height", utils.toPx(newHeight));
            data.canvas.style.top = utils.toPx(yPadding);
            data.canvas.style.left = utils.toPx(xPadding);
        }

        function positionFooter() {
            data.footer.style.position = "fixed";
            data.footer.style.width = utils.toPx(window.innerWidth);
            data.footer.style.top = utils.toPx(window.innerHeight - data.footerHeight);
            data.footer.style.left = utils.toPx(0);
        }

        function logicalToVisual(xy) {
            return new vecMath.vector2d(
                Math.round(data.canvas.width / 2 + xy.x * data.logicalToVisualFactor),
                Math.round(data.canvas.height / 2 - xy.y * data.logicalToVisualFactor)
            );
        }

        function visualToLogic(xy) {
            return new vecMath.vector2d(
                Math.round((xy.x - data.canvas.width / 2) / data.logicalToVisualFactor),
                Math.round((xy.y - data.canvas.height / 2) * -1 / data.logicalToVisualFactor)
            );
        }

        function startTalking() {
            data.talking = true;
            data.talkedit.style.display = "block";
            data.talkedit.style.position = "fixed";
            data.talkedit.style.top = data.canvas.style.top;//utils.toPx(utils.fromPx(data.canvas.style.top) + data.canvas.height - data.talkedit.height);
            data.talkedit.style.left = data.canvas.style.left;
            // [Randy 08/10/2010] TODO: Determine the number of columns correctly.
            data.talkedit.setAttribute("cols", data.canvas.width / 9);
            data.talkedit.focus();
        }

        function stopTalking() {
            data.talking = false;
            data.talkedit.style.display = "none";
            var message = utils.removeTrailingEnter(data.talkedit.value);
            if (message != "") {
                communication.send({
                                       "Function": "talk",
                                       "Params": {
                                           "Message": message
                                       }
                                   });
            }
            data.talkedit.value = "";
        }

        function drawSvgAround(cxt, key, x, y) {
            // TODO: Find a way of determining the width/height of a svg.
            var svgWidth = 64;
            var svgHeight = 64;
            cxt.drawSvg(resourceBuffer.get(key), x - svgWidth / 2, y - svgHeight / 2);
        }

        function drawSvgAt(cxt, key, x, y) {
            cxt.drawSvg(resourceBuffer.get(key), x, y);
        }

        function drawCanvas() {
            var cxt = data.canvas.getContext("2d");
            var topLeft = visualToLogic(new vecMath.vector2d(0.0, 0.0));
            var bottomRight = visualToLogic(new vecMath.vector2d(data.canvas.width - 1, data.canvas.height - 1));

            cxt.fillStyle = "#88FF88";
            cxt.fillRect(0, 0, data.canvas.width - 1, data.canvas.height - 1);

            for (var x = topLeft.x - 1; x < bottomRight.x + 1; x++) {
                for (var y = bottomRight.y - 1; y < topLeft.y + 1; y++) {
                    var rcCenter = logicalToVisual(new vecMath.vector2d(x, y));
                    var halfTile = data.logicalToVisualFactor * 0.5;
                    var rcTopLeft = vecMath.subtract(rcCenter, new vecMath.vector2d(halfTile, halfTile));
                    cxt.strokeStyle = "#AAAAAA";
                    cxt.strokeRect(rcTopLeft.x, rcTopLeft.y, data.logicalToVisualFactor, data.logicalToVisualFactor);
                }
            }

            cxt.fillStyle = "#FF0000";
            var playerLogicalVisual = logicalToVisual(data.playerLogicSide);
            cxt.fillRect(playerLogicalVisual.x - 2, playerLogicalVisual.y - 2, 4, 4);

            var playerUiVisual = logicalToVisual(data.playerUiSide);
            drawSvgAround(cxt, "entities/player", playerUiVisual.x, playerUiVisual.y);

            drawSvgAt(cxt, "hud/talk", 20, 20);
        }

        function updateUiData() {
            var vm = vecMath;
            var newUpdateTime = data.now();
            // [Randy 06/10/2010] REMARK: Speed is 1 tile / second.
            var distance = (newUpdateTime - data.lastUpdateTime) * 0.001;
            var v = vm.subtract(data.playerLogicSide, data.playerUiSide);
            var vLength = v.length();
            // [Randy 06/10/2010] REMARK: If length becomes small (like 0.01)
            // we seem to get rounding issues (jittering).
            if (vLength > 0.1) {
                data.playerUiSide = vm.add(data.playerUiSide, vm.scale(v, distance / vLength));
            } else {
                data.playerUiSide = data.playerLogicSide;
            }
            data.lastUpdateTime = newUpdateTime;
        }

        function onCanvasClick(evt) {
            var offsetX = utils.onclickOffset(evt, "X", data.canvas);
            var offsetY = utils.onclickOffset(evt, "Y", data.canvas);
            var newPosition = visualToLogic(new vecMath.vector2d(offsetX, offsetY));
            communication.send({
                                   "Function": "move",
                                   "Params": {
                                       "X": "" + newPosition.x,
                                       "Y": "" + newPosition.y
                                   }
                               });
        }

        function onCanvasKeyup(evt) {
            var keyunicode = utils.onkeyKey(evt);
            //alert(keyunicode);
            if (keyunicode == 84 /* t */) {
                if (!data.taking) {
                    startTalking();
                }
            }
            if (keyunicode == 13 /* enter */) {
                if (data.talking) {
                    stopTalking();
                }
            }
        }

        function layoutPage() {
            positionCanvas();
            positionFooter();
        }

        function onWindowLoad() {
            data.canvas = document.getElementById("maincanvas");
            data.talkedit = document.getElementById("talkedit");
            data.footer = document.getElementById("footer");
            resourceBuffer.addXml("hud/talk", "images/hud/talk.svg");
            resourceBuffer.addXml("entities/player", "images/faces/human/human01.svg");
            layoutPage();
            var fps = 30;
            setInterval(updateUiData, 1000 / fps);
            setInterval(drawCanvas, 1000 / fps);
            data.canvas.onclick = onCanvasClick;
            // [Randy 08/10/2010] REMARK: Attaching to the canvas doesn't seem to work.
            document.onkeyup = onCanvasKeyup;
        }

        function onWindowResize() {
            layoutPage();
            drawCanvas();
        }

        return {
            onWindowLoad: onWindowLoad,
            onWindowResize: onWindowResize
        };

    }();

    //##############################################################################################

    var testing = function() {

        function test(testString, condition) {
            if (!condition) {
                log("test failed: " + testString + "!");
            }
        }

        function runTests() {
            test("toPx", (utils.toPx(10) == "10px"));
            test("fromPx", (utils.fromPx("10px") == "10"));
            test("lerp1", (utils.lerp(0.0, 10.0, 0.5) == 5.0));
            test("lerp2", (utils.lerp(10.0, 30.0, 0.5) == 20.0));
            test("lerp3", (utils.lerp(10.0, 30.0, 0.0) == 10.0));
            test("lerp4", (utils.lerp(10.0, 30.0, 1.0) == 30.0));
            test("removeTrailingEnter", (utils.removeTrailingEnter("test\n") == "test"));
        }

        return {
            runTests: runTests
        };

    }();

    return {
        main: main,
        testing: testing
    };

}();

//##################################################################################################

var runtests = false;
if (!runtests) {
    window.onload = musq.main.onWindowLoad;
    window.onresize = musq.main.onWindowResize;
}
else {
    window.onload = musq.testing.runTests;
}
