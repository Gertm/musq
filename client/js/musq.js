var musq = function () {

    //## generic utilities that have no dependencies ###############################################

    var utils = function () {

        function toPx(i) {
            return i + "px";
        }

        function fromPx(s) {
            return parseInt(s.substr(0, s.length - 2), 10);
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
            if (s === "") {
                return s;
            }
            if (s.charAt(s.length - 1) == '\n') {
                return s.substr(0, s.length - 1);
            }
            return s;
        }

        function ptInRc(rc, pt) {
            if (pt.x < rc.x) {
                return false;
            }
            if (pt.x >= rc.x + rc.width) {
                return false;
            }
            if (pt.y < rc.y) {
                return false;
            }
            if (pt.y >= rc.y + rc.height) {
                return false;
            }
            return true;
        }

        function inherit(instance, baseConstructor) {
            var base = new baseConstructor();
            for (var member in base) {
                instance[member] = base[member];
            }
        }

        function now() {
            return (new Date()).getTime();
        }

        return {
            toPx: toPx,
            fromPx: fromPx,
            lerp: lerp,
            onclickOffset: onclickOffset,
            onkeyKey: onkeyKey,
            removeTrailingEnter: removeTrailingEnter,
            ptInRc: ptInRc,
            inherit: inherit,
            now: now
        };

    } ();

    function log(txt) {
        if (console.log)
            console.log("MUSQ: " + txt);
    }

    //## 2D vector math utilities ##################################################################

    var vecMath = function () {

        function vector2d(x, y) {
            this.x = x;
            this.y = y;
            this.length = function () {
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
        }

        return {
            vector2d: vector2d,
            add: add,
            subtract: subtract,
            scale: scale,
            normalize: normalize
        };

    } ();

    //## animation classes #########################################################################

    function moveAnimation() {
        this.curr = new vecMath.vector2d();
        this.dst = new vecMath.vector2d();
        this.speed = 1.0;
        this.initialize = function (position) {
            this.curr = position;
            this.dst = position;
        };
        this.setDestination = function (dst, timeToReachInSecs) {
            this.dst = dst;
            this.speed = vecMath.subtract(this.dst, this.curr).length() / timeToReachInSecs;
        };
        this.update = function (timeDiffInMilliSecs) {
            var distance = this.speed * timeDiffInMilliSecs * 0.001;
            var v = vecMath.subtract(this.dst, this.curr);
            var vLength = v.length();
            // [Randy 06/10/2010] REMARK: If length becomes small (like 0.01)
            // we seem to get rounding issues (jittering).
            if (vLength > 0.1) {
                this.curr = vecMath.add(this.curr, vecMath.scale(v, distance / vLength));
            } else {
                this.curr = this.dst;
            }
        };
    }

    //## HUD elements ##############################################################################

    function gameHudElement() {
        this.x = 0;
        this.y = 0;
        this.width = 0;
        this.height = 0;
        this.draw = function (cxt) { };
        this.onClick = function () { };
        this.hitTest = function (pt) {
            var rc = {
                x: this.x,
                y: this.y,
                width: this.width,
                height: this.height
            };
            return utils.ptInRc(rc, pt);
        };
        this.onMouseClick = function (pt) {
            if (this.hitTest(pt)) {
                this.onClick();
                return true;
            }
            return false;
        };
    }

    function gameHudImageElement(image) {
        utils.inherit(this, gameHudElement);
        this.width = image.width;
        this.height = image.height;
        this.draw = function (cxt) {
            cxt.drawImage(image, this.x, this.y);
        };
    }

    //## canvas elements ###########################################################################

    function gameEntity(key) {
        this.key = key;
        this.moveAnimation = new moveAnimation();
    }

    //## global data ###############################################################################

    var data = {};
    data.state = "init";
    data.login = {};
    data.login.lastUpdateTime = utils.now();
    data.login.scaleFactor = 1.0;
    data.login.scaleDirection = -1.0;
    data.game = {};
    data.game.fps = 30;
    data.game.viewPortCenter = new vecMath.vector2d(0.0, 0.0);
    data.game.lastUpdateTime = utils.now();
    data.game.talking = false;
    data.game.logicalToVisualFactor = 70.0;
    data.game.entities = [];

    //## image/resource buffer management ##########################################################

    var resourceBuffer = function () {

        var container = {};

        function addImage(key, url) {
            var image = new Image();
            image.src = url;
            container[key] = image;
        }

        function addSvgs(key, urls) {
            var canvas = document.getElementById("svg2pngcanvas");
            var cxt = canvas.getContext("2d");
            cxt.clearRect(0, 0, canvas.width, canvas.height);
            urls.forEach(function (url, index, array) {
                             var xmlHttp = new XMLHttpRequest();
                             xmlHttp.open("GET", url, false);
                             xmlHttp.send();
                             var svg = xmlHttp.responseXML.getElementsByTagName("svg")[0];
                             var width = parseInt(svg.getAttribute("width"), 10);
                             var height = parseInt(svg.getAttribute("height"), 10);
                             if (canvas.width !== width || canvas.height !== height) {
                                 canvas.setAttribute("width", utils.toPx(width));
                                 canvas.setAttribute("height", utils.toPx(height));
                             }
                             cxt.drawSvg(xmlHttp.responseText, 0, 0);
                         });
            addImage(key, canvas.toDataURL());
        }

        function addSvg(key, url) {
            addSvgs(key, [url]);
        }

        function remove(key) {
            container[key] = undefined;
        }

        function get(key) {
            return container[key];
        }

        return {
            addImage: addImage,
            addSvg: addSvg,
            addSvgs: addSvgs,
            remove: remove,
            get: get
        };

    } ();

    //## utilities that depend on the global data ##################################################

    function logicalToVisual(xy) {
        var x = Math.round(data.game.canvas.width / 2 + xy.x * data.game.logicalToVisualFactor);
        var y = Math.round(data.game.canvas.height / 2 - xy.y * data.game.logicalToVisualFactor);
        return new vecMath.vector2d(x, y);
    }

    function visualToLogic(xy) {
        var x = Math.round((xy.x - data.game.canvas.width / 2) / data.game.logicalToVisualFactor);
        var y = Math.round((xy.y - data.game.canvas.height / 2) * -1 / data.game.logicalToVisualFactor);
        return new vecMath.vector2d(x, y);
    }

    function drawImageAroundUi(cxt, key, pt) {
        var image = resourceBuffer.get(key);
        var width = image.width;
        var height = image.height;
        cxt.drawImage(image, pt.x - width / 2, pt.y - height / 2);
    }

    function drawImageAroundLogic(cxt, key, pt) {
        drawImageAroundUi(cxt, key, logicalToVisual(pt));
    }

    function drawImageAtUi(cxt, key, pt) {
        cxt.drawImage(resourceBuffer.get(key), pt.x, pt.y);
    }

    function drawImageAtLogic(cxt, key, pt) {
        drawImageAtUi(cxt, key, logicalToVisual(pt));
    }

    function wsSend(obj) {
        data.ws.send(JSON.stringify(obj));
    }

    function sendKeepAliv(obj) {
        wsSend({
                   "Function": "keepalive",
                   "Params": {}
               });
    }

    //## drawing ###################################################################################

    function drawLoginCanvas() {
        if (data.state !== "login") {
            return;
        }
        var cxt = data.login.canvas.getContext("2d");
        cxt.save();
        cxt.clearRect(0, 0, data.login.canvas.width, data.login.canvas.height);
        var image = resourceBuffer.get("login/logo");
        var newWidth = image.width * data.login.scaleFactor;
        cxt.translate((image.width - newWidth) / 2.0, 0.0);
        cxt.scale(data.login.scaleFactor, 1.0);
        cxt.drawImage(image, 0.0, 0.0);
        cxt.restore();
    }

    function drawGameBackground(cxt) {
        cxt.fillStyle = "#88FF88";
        cxt.fillRect(0, 0, data.game.canvas.width - 1, data.game.canvas.height - 1);
    }

    function drawGameGrid(cxt) {
        var topLeft = visualToLogic(new vecMath.vector2d(0.0, 0.0));
        var bottomRight = visualToLogic(new vecMath.vector2d(data.game.canvas.width - 1, data.game.canvas.height - 1));
        for (var x = topLeft.x - 1; x < bottomRight.x + 1; x++) {
            for (var y = bottomRight.y - 1; y < topLeft.y + 1; y++) {
                var rcCenter = logicalToVisual(new vecMath.vector2d(x, y));
                var halfTile = data.game.logicalToVisualFactor * 0.5;
                var rcTopLeft = vecMath.subtract(rcCenter, new vecMath.vector2d(halfTile, halfTile));
                cxt.strokeStyle = "#AAAAAA";
                cxt.strokeRect(rcTopLeft.x, rcTopLeft.y, data.game.logicalToVisualFactor, data.game.logicalToVisualFactor);
            }
        }
    }

    function drawGameMoveTarget(cxt) {
        cxt.fillStyle = "#FF0000";
        var pt = logicalToVisual(data.game.player.moveAnimation.dst);
        cxt.fillRect(pt.x - 2, pt.y - 2, 4, 4);
    }

    function drawGameEntities(cxt) {
        data.game.entities.forEach(function (e, index, array) {
                                       drawImageAroundLogic(cxt, e.key, e.moveAnimation.curr);
                                   });
    }

    function drawGameHud(cxt) {
        data.game.hudTalk.draw(cxt);
    }

    function drawGameCanvas() {
        if (data.state !== "game") {
            return;
        }
        var cxt = data.game.canvas.getContext("2d");
        drawGameBackground(cxt);
        drawGameGrid(cxt);
        drawGameMoveTarget(cxt);
        drawGameEntities(cxt);
        drawGameHud(cxt);
    }

    //## updating ##################################################################################

    function updateLoginAnimation() {
        if (data.state !== "login") {
            return;
        }
        var newUpdateTime = utils.now();
        data.login.scaleFactor += data.login.scaleDirection * (newUpdateTime - data.login.lastUpdateTime) * 0.001 * 0.3;
        if (data.login.scaleFactor > 1.0) {
            data.login.scaleFactor = 1.0;
            data.login.scaleDirection = -1.0;
        }
        if (data.login.scaleFactor < -1.0) {
            data.login.scaleFactor = -1.0;
            data.login.scaleDirection = 1.0;
        }
        data.login.lastUpdateTime = newUpdateTime;
    }

    function updateGameUiData() {
        if (data.state !== "game") {
            return;
        }
        var newUpdateTime = utils.now();
        var timeDiffInMs = newUpdateTime - data.game.lastUpdateTime;
        data.game.entities.forEach(function (e, index, array) {
                                       e.moveAnimation.update(timeDiffInMs);
                                   });
        data.game.lastUpdateTime = newUpdateTime;
    }

    //## message handlers ##########################################################################

    function setStateToLogin() {
        data.login.container.style.display = "block";
        data.game.container.style.display = "none";
        data.state = "login";
        data.login.username.focus();
    }

    function setStateToGame() {
        data.login.container.style.display = "none";
        data.game.container.style.display = "block";
        data.state = "game";
    }

    function startTalking() {
        data.game.talking = true;
        data.game.talkedit.style.display = "block";
        data.game.talkedit.style.position = "fixed";
        data.game.talkedit.style.top = utils.toPx(utils.fromPx(data.game.canvas.style.top) + data.game.canvas.height - data.game.talkedit.clientHeight);
        data.game.talkedit.style.left = data.game.canvas.style.left;
        // [Randy 08/10/2010] TODO: Determine the number of columns correctly.
        data.game.talkedit.setAttribute("cols", Math.round(data.game.canvas.width / 8.2));
        data.game.talkedit.focus();
    }

    function sendTalkMessage() {
        var message = utils.removeTrailingEnter(data.game.talkedit.value);
        if (message !== "") {
            wsSend({
                       "Function": "talk",
                       "Params": {
                           "Message": message
                       }
                   });
        }
        data.game.talkedit.value = "";
    }

    function stopTalking() {
        data.game.talking = false;
        data.game.talkedit.style.display = "none";
    }

    function onLoginButton() {
        wsSend({
                   "Function": "login",
                   "Params": {
                       "Username": data.login.username.value,
                       "Password": data.login.password.value
                   }
               });
    }

    function onGameCanvasClick(evt) {
        var offsetX = utils.onclickOffset(evt, "X", data.game.canvas);
        var offsetY = utils.onclickOffset(evt, "Y", data.game.canvas);
        var pt = new vecMath.vector2d(offsetX, offsetY);
        if (data.game.hudTalk.onMouseClick(pt)) {
            return;
        }
        var newPosition = visualToLogic(pt);
        wsSend({
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
        if (data.state === "login") {
            if (keyunicode == 13 /* enter */) {
                onLoginButton();
                return;
            }
            return;
        }
        if (data.state === "game") {
            if (keyunicode == 84 /* t */) {
                if (!data.game.talking) {
                    startTalking();
                }
                return;
            }
            if (keyunicode == 13 /* enter */) {
                if (data.game.talking) {
                    sendTalkMessage();
                    stopTalking();
                }
                return;
            }
            return;
        }
    }

    function onGameHudTalkClick() {
        if (!data.game.talking) {
            startTalking();
        } else {
            stopTalking();
        }
    }

    function onWebSocketOpened() {
        log("WebSocket opened.");
        setInterval(sendKeepAliv, 10000);
    }

    function onWebSocketClosed() {
        log("WebSocket closed.");
        // [Randy 14/10/2010] TODO: Reset the state.
    }

    function onWebSocketMessage(evt) {
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
            setStateToGame();
            return;
        }
        if (json.Function == "keepalive") {
            return;
        }
        if (json.Function == "move") {
            var newDestination = new vecMath.vector2d(parseInt(json.Params.X, 10), parseInt(json.Params.Y, 10));
            data.game.player.moveAnimation.setDestination(newDestination, 1.0);
            return;
        }
        if (json.Function == "talk") {
            alert(json.Params.Message);
            return;
        }
    }

    //## page layout ###############################################################################

    function positionLogin() {
        data.login.container.style.position = "fixed";
        var width = 500;
        data.login.container.style.width = utils.toPx(width);
        var height = 230;
        data.login.container.style.height = utils.toPx(height);
        data.login.container.style.top = utils.toPx((window.innerHeight - height) / 2);
        data.login.container.style.left = utils.toPx((window.innerWidth - width) / 2);
        data.login.controls.style.marginTop = utils.toPx((data.login.container.clientHeight - data.login.controls.clientHeight) / 2);
    }

    function positionGameCanvas() {
        var xPadding = 40;
        var yPadding = 20;
        data.game.canvas.style.position = "fixed";
        var newWidth = window.innerWidth - xPadding * 2;
        data.game.canvas.setAttribute("width", utils.toPx(newWidth));
        var newHeight = window.innerHeight - yPadding * 2 - data.footer.clientHeight;
        data.game.canvas.setAttribute("height", utils.toPx(newHeight));
        data.game.canvas.style.top = utils.toPx(yPadding);
        data.game.canvas.style.left = utils.toPx(xPadding);
    }

    function positionFooter() {
        data.footer.style.position = "fixed";
        data.footer.style.width = utils.toPx(window.innerWidth);
        data.footer.style.top = utils.toPx(window.innerHeight - data.footer.clientHeight);
        data.footer.style.left = utils.toPx(0);
    }

    function layoutPage() {
        positionLogin();
        positionGameCanvas();
        positionFooter();
    }

    function layoutGameHud() {
        data.game.hudTalk.x = 20;
        data.game.hudTalk.y = 20;
    }

    //## initialization ############################################################################

    function preloadResources() {
        // [Randy 14/10/2010] PATCH: logo.svg causes an exception in canvg.
        // Maybe because the font doesn't exist here?
        //resourceBuffer.addSvg("login/logo", "images/logo.svg");
        resourceBuffer.addImage("login/logo", "images/logo.png");
        resourceBuffer.addSvg("hud/talk", "images/hud/talk.svg");
        resourceBuffer.addSvgs(
            "entities/player",
            ["images/faces/human/male/face01.svg",
             "images/faces/human/male/ears01.svg",
             "images/faces/human/male/eyes01.svg",
             "images/faces/human/male/hair01.svg",
             "images/faces/human/male/mouth01.svg",
             "images/faces/human/male/nose01.svg"]);
        resourceBuffer.addSvgs(
            "entities/enemy01",
            ["images/faces/human/male/face02.svg",
             "images/faces/human/male/ears02.svg",
             "images/faces/human/male/eyes01.svg",
             "images/faces/human/male/mouth01.svg",
             "images/faces/human/male/nose01.svg"]);
    }

    function initializeGameHud() {
        data.game.hudTalk = new gameHudImageElement(resourceBuffer.get("hud/talk"));
        data.game.hudTalk.onClick = onGameHudTalkClick;
    }

    function initializeGameEntities() {
        data.game.player = new gameEntity("entities/player");
        data.game.player.moveAnimation.initialize(new vecMath.vector2d(0.0, 0.0));
        data.game.entities.push(data.game.player);
        data.game.enemy01 = new gameEntity("entities/enemy01");
        data.game.enemy01.moveAnimation.initialize(new vecMath.vector2d(-3.0, 3.0));
        data.game.entities.push(data.game.enemy01);
    }

    function initializeWebSocket() {
        data.ws = new WebSocket("ws://" + musq_websocket_url + "/service");
        data.ws.onopen = onWebSocketOpened;
        data.ws.onclose = onWebSocketClosed;
        data.ws.onmessage = onWebSocketMessage;
    }

    function initializeLogin() {
        data.login.container = document.getElementById("logincontainer");
        data.login.canvas = document.getElementById("logincanvas");
        data.login.controls = document.getElementById("logincontrols");
        data.login.username = document.getElementById("loginusername");
        data.login.password = document.getElementById("loginpassword");
        data.login.button = document.getElementById("loginbutton");
        data.login.button.onclick = onLoginButton;
        setInterval(updateLoginAnimation, 30);
        setInterval(drawLoginCanvas, 30);
    }

    function initializeGame() {
        data.game.container = document.getElementById("gamecontainer");
        data.game.canvas = document.getElementById("gamecanvas");
        data.game.talkedit = document.getElementById("gametalkedit");
        initializeGameHud();
        initializeGameEntities();
        setInterval(updateGameUiData, 1000 / data.game.fps);
        setInterval(drawGameCanvas, 1000 / data.game.fps);
        data.game.canvas.onclick = onGameCanvasClick;
    }

    function onWindowLoad() {
        data.footer = document.getElementById("footer");
        preloadResources();
        initializeLogin();
        initializeGame();
        // [Randy 08/10/2010] REMARK: Attaching to the canvas doesn't seem to work.
        document.onkeyup = onCanvasKeyup;
        setStateToLogin();
        initializeWebSocket();
        layoutPage();
    }

    function onWindowResize() {
        layoutPage();
        layoutGameHud();
        drawGameCanvas();
    }

    //## testing ###################################################################################

    var testing = function () {

        function test(testString, condition) {
            if (!condition) {
                alert("test failed: " + testString + "!");
            }
        }

        function runTests() {
            test("toPx", (utils.toPx(10) == "10px"));
            test("fromPx", (utils.fromPx("10px") == 10));
            test("lerp1", (utils.lerp(0.0, 10.0, 0.5) == 5.0));
            test("lerp2", (utils.lerp(10.0, 30.0, 0.5) == 20.0));
            test("lerp3", (utils.lerp(10.0, 30.0, 0.0) == 10.0));
            test("lerp4", (utils.lerp(10.0, 30.0, 1.0) == 30.0));
            test("removeTrailingEnter", (utils.removeTrailingEnter("test\n") == "test"));
            test("ptInRc1", (utils.ptInRc({ x: 50, y: 50, width: 50, height: 50 }, { x: 60, y: 60 })));
            test("ptInRc2", (!utils.ptInRc({ x: 50, y: 50, width: 50, height: 50 }, { x: 40, y: 60 })));
        }

        return {
            runTests: runTests
        };

    } ();

    return {
        onWindowLoad: onWindowLoad,
        onWindowResize: onWindowResize,
        testing: testing
    };

} ();

//##################################################################################################

var runtests = false;
if (!runtests) {
    window.onload = musq.onWindowLoad;
    window.onresize = musq.onWindowResize;
}
else {
    window.onload = musq.testing.runTests;
}
