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

        function stringReplaceRecursive(s, oldS, newS) {
            var r = s.replace(oldS, newS);
            if (r === s)
                return r;
            return stringReplaceRecursive(r, oldS, newS);
        }

        function expandRc(rc, xborder, yborder) {
            return {
                x: rc.x - xborder,
                y: rc.y - yborder,
                width: rc.width + 2 * xborder,
                height: rc.height + 2 * yborder
            };
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
            now: now,
            stringReplaceRecursive: stringReplaceRecursive,
            expandRc: expandRc
        };

    } ();

    function log(txt) {
        if (console.log)
            console.log("MUSQ: " + txt);
    }

    String.prototype.replaceAll = function (oldS, newS) {
        return utils.stringReplaceRecursive(this, oldS, newS);
    };

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
        this.curr = new vecMath.vector2d(0.0, 0.0);
        this.dst = new vecMath.vector2d(0.0, 0.0);
        this.speed = 1.0;
        this.initialize = function (position) {
            this.curr = new vecMath.vector2d(position.x, position.y);
            this.dst = new vecMath.vector2d(position.x, position.y);
        };
        this.setDestination = function (dst, timeToReachInSecs) {
            this.dst = new vecMath.vector2d(dst.x, dst.y);
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
                this.curr = new vecMath.vector2d(this.dst.x, this.dst.y);
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

    function gameEntity() {
        this.moveAnimation = new moveAnimation();
        this.messages = [];
    }

    //## global data ###############################################################################

    var data = {};
    data.onWindowLoaded = false;
    data.state = "init";
    data.playerName = "";
    data.login = {};
    data.login.lastUpdateTime = utils.now();
    data.login.scaleFactor = 1.0;
    data.login.scaleDirection = -1.0;
    data.game = {};
    data.game.fps = 30;
    data.game.viewPortCenter = new moveAnimation();
    data.game.lastUpdateTime = utils.now();
    data.game.talking = false;
    data.game.logicalToVisualFactor = 70.0;
    data.game.entities = {};
    data.game.colors = {};
    data.game.colors["skin"] = ["#fff0c1", "#785d42"];
    data.game.colors["scar"] = ["#8c846a", "#362a1e"];
    data.game.colors["hair"] = ["#140d00"];
    data.game.colors["eyes"] = ["#000000", "#000044"];

    //## image/resource buffer management ##########################################################

    var resourceBuffer = function () {

        var container = {};

        function addImage(key, url) {
            var image = new Image();
            image.src = url;
            container[key] = image;
        }

        function addSvgs(key, urlsAndColors) {
            var canvas = document.getElementById("svg2pngcanvas");
            var cxt = canvas.getContext("2d");
            cxt.clearRect(0, 0, canvas.width, canvas.height);
            urlsAndColors.forEach(function (e, index, array) {
                                      var xmlHttp = new XMLHttpRequest();
                                      xmlHttp.open("GET", e.Url, false);
                                      xmlHttp.send();
                                      var svg = xmlHttp.responseXML.getElementsByTagName("svg")[0];
                                      var width = parseInt(svg.getAttribute("width"), 10);
                                      var height = parseInt(svg.getAttribute("height"), 10);
                                      if (canvas.width !== width || canvas.height !== height) {
                                          canvas.setAttribute("width", utils.toPx(width));
                                          canvas.setAttribute("height", utils.toPx(height));
                                      }
                                      var svgTxt = e.Color ? xmlHttp.responseText.replaceAll("#badf0d", e.Color) : xmlHttp.responseText;
                                      cxt.drawSvg(svgTxt, 0, 0);
                                  });
            addImage(key, canvas.toDataURL());
        }

        function addSvg(key, url, color) {
            addSvgs(key, [{"Url": url, "Color": color}]);
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
        var viewPortCenter = data.game.viewPortCenter.curr;
        var x = Math.round(data.game.canvas.width / 2 + (xy.x - viewPortCenter.x) * data.game.logicalToVisualFactor);
        var y = Math.round(data.game.canvas.height / 2 - (xy.y - viewPortCenter.y) * data.game.logicalToVisualFactor);
        return new vecMath.vector2d(x, y);
    }

    function visualToLogic(xy) {
        var viewPortCenter = data.game.viewPortCenter.curr;
        var x = Math.round((xy.x - data.game.canvas.width / 2) / data.game.logicalToVisualFactor + viewPortCenter.x);
        var y = Math.round((xy.y - data.game.canvas.height / 2) * -1 / data.game.logicalToVisualFactor + viewPortCenter.y);
        return new vecMath.vector2d(x, y);
    }

    function getLogicalViewPort() {
        return {
            topLeft: visualToLogic(new vecMath.vector2d(0.0, 0.0)),
            bottomRight: visualToLogic(new vecMath.vector2d(data.game.canvas.width - 1, data.game.canvas.height - 1))
        };
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
        var viewPort = getLogicalViewPort();
        for (var x = viewPort.topLeft.x - 1; x < viewPort.bottomRight.x + 1; x++) {
            for (var y = viewPort.bottomRight.y - 1; y < viewPort.topLeft.y + 1; y++) {
                var rcCenter = logicalToVisual(new vecMath.vector2d(x, y));
                var halfTile = data.game.logicalToVisualFactor * 0.5;
                var rcTopLeft = vecMath.subtract(rcCenter, new vecMath.vector2d(halfTile, halfTile));
                cxt.strokeStyle = "#AAAAAA";
                cxt.strokeRect(rcTopLeft.x, rcTopLeft.y, data.game.logicalToVisualFactor, data.game.logicalToVisualFactor);
            }
        }
    }

    function drawGameMoveTarget(cxt) {
        if (data.playerName === "" || !data.game.entities[data.playerName]) {
            return;
        }
        cxt.fillStyle = "#FF0000";
        var pt = logicalToVisual(data.game.entities[data.playerName].moveAnimation.dst);
        cxt.fillRect(pt.x - 2, pt.y - 2, 4, 4);
    }

    function drawTalk(cxt, entityPtLogic, message) {
        var entityPtUi = logicalToVisual(entityPtLogic);
        var txtoffset = { x: data.game.logicalToVisualFactor * 0.3, y: -data.game.logicalToVisualFactor * 0.4 };
        var txtpt = vecMath.add(entityPtUi, txtoffset);
        var txtwidth = cxt.measureText(message).width;
        var textLineHeight = 15;
        var txtrc = {
            x: txtpt.x,
            y: txtpt.y - textLineHeight,
            width: txtwidth,
            height: textLineHeight
        };
        var backrc = utils.expandRc(txtrc, 5, 8);
        var drawBackPath = function () {
            var corner = 8;
            cxt.beginPath();
            cxt.moveTo(backrc.x, backrc.y + corner);
            cxt.arcTo(backrc.x, backrc.y, backrc.x + corner, backrc.y, corner);
            cxt.lineTo(backrc.x + backrc.width - corner, backrc.y);
            cxt.arcTo(backrc.x + backrc.width, backrc.y, backrc.x + backrc.width, backrc.y + corner, corner);
            cxt.lineTo(backrc.x + backrc.width, backrc.y + backrc.height - corner);
            cxt.arcTo(backrc.x + backrc.width, backrc.y + backrc.height, backrc.x + backrc.width - corner, backrc.y + backrc.height, corner);
            cxt.lineTo(backrc.x + corner + 10, backrc.y + backrc.height);
            cxt.lineTo(backrc.x + corner + 5, backrc.y + backrc.height + 10);
            cxt.lineTo(backrc.x + corner + 5, backrc.y + backrc.height);
            cxt.lineTo(backrc.x + corner, backrc.y + backrc.height);
            cxt.arcTo(backrc.x, backrc.y + backrc.height, backrc.x, backrc.y + backrc.height - corner, corner);
            cxt.lineTo(backrc.x, backrc.y + corner);
        };
        cxt.save();
        cxt.fillStyle = "#FFFFFF";
        cxt.shadowOffsetX = 4;
        cxt.shadowOffsetY = 4;
        cxt.shadowBlur = 3;
        cxt.shadowColor = "#222222";
        drawBackPath();
        cxt.fill();
        cxt.restore();
        cxt.strokeStyle = "#000000";
        drawBackPath();
        cxt.stroke();
        cxt.font = textLineHeight + "px SmackAttackBB";
        cxt.textAlign = "left";
        cxt.textBaseline = "bottom";
        cxt.fillStyle = "#000000";
        cxt.fillText(message, txtpt.x, txtpt.y);
    }

    function drawGameEntities(cxt) {
        for (eI in data.game.entities) {
            var e = data.game.entities[eI];
            if (e.imageKey) {
                drawImageAroundLogic(cxt, e.imageKey, e.moveAnimation.curr);
            }
            if (e.messages.length !== 0) {
                drawTalk(cxt, e.moveAnimation.curr, e.messages[0]);
            }
        }
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
        data.game.viewPortCenter.update(timeDiffInMs);
        for (eI in data.game.entities) {
            var e = data.game.entities[eI];
            e.moveAnimation.update(timeDiffInMs);
        }
        data.game.lastUpdateTime = newUpdateTime;
    }

    //## message handler helpers ###################################################################

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

    function clearTalkMessage(playerName) {
        var player = data.game.entities[playerName];
        if (!player)
            return;
        if (player.messages.length === 0)
            return;
        player.messages = player.messages.slice(1);
    }

    function ensurePlayerIsWithinViewPort() {
        var playerDst = data.game.entities[data.playerName].moveAnimation.dst;
        var viewPort = getLogicalViewPort();
        var border = 3;
        var newViewPortCenter = new vecMath.vector2d(data.game.viewPortCenter.dst.x, data.game.viewPortCenter.dst.y);
        if (playerDst.x > viewPort.bottomRight.x - border) {
            newViewPortCenter.x = playerDst.x;
        }
        if (playerDst.x < viewPort.topLeft.x + border) {
            newViewPortCenter.x = playerDst.x;
        }
        if (playerDst.y > viewPort.topLeft.y - border) {
            newViewPortCenter.y = playerDst.y;
        }
        if (playerDst.y < viewPort.bottomRight.y + border) {
            newViewPortCenter.y = playerDst.y;
        }
        data.game.viewPortCenter.setDestination(newViewPortCenter, 1.0);
    }

    function setLoginIncorrect() {
        data.login.username.style.backgroundColor = "#FF0000";
        data.login.password.style.backgroundColor = "#FF0000";
        data.login.username.focus();
    }

    function handleLoginJson(json) {
        if (json.Params.Success === "true") {
            data.playerName = data.login.username.value;
            data.login.username.style.backgroundColor = "#FFFFFF";
            data.login.password.style.backgroundColor = "#FFFFFF";
            data.game.entities = {};
            setStateToGame();
        } else {
            setLoginIncorrect();
        }
    }

    function handleVisualJson(json) {
        var player = new gameEntity();
        player.imageKey = "entities/" + json.Params.Name;
        resourceBuffer.addSvgs(player.imageKey, json.Params.Images);
        data.game.entities[json.Params.Name] = player;
    }

    function handleJumpJson(json) {
        var player = data.game.entities[json.Params.Name];
        if (!player) {
            return;
        }
        var pos = new vecMath.vector2d(parseInt(json.Params.X, 10), parseInt(json.Params.Y, 10));
        player.moveAnimation.initialize(pos);
        if (json.Params.Name === data.playerName) {
            data.game.viewPortCenter.initialize(pos);
        }
    }

    function handleMoveJson(json) {
        var newDestination = new vecMath.vector2d(parseInt(json.Params.X, 10), parseInt(json.Params.Y, 10));
        var player = data.game.entities[json.Params.Name];
        if (!player) {
            return;
        }
        player.moveAnimation.setDestination(newDestination, 1.0);
        if (json.Params.Name === data.playerName) {
            ensurePlayerIsWithinViewPort();
        }
    }

    function handleTalkJson(json) {
        var player = data.game.entities[json.Params.Name];
        if (!player) {
            return;
        }
        player.messages.push(json.Params.Message);
        setTimeout(function () { clearTalkMessage(json.Params.Name); }, 3000);
    }

    //## message handlers ##########################################################################

    function onLoginButton() {
        if (data.login.username.value === "") {
            setLoginIncorrect();
            return;
        }
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
        setStateToLogin();
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
        if (json.Function === "login") {
            handleLoginJson(json);
            return;
        }
        if (json.Function === "jump") {
            handleJumpJson(json);
            return;
        }
        if (json.Function === "keepalive") {
            return;
        }
        if (json.Function === "move") {
            handleMoveJson(json);
            return;
        }
        if (json.Function === "talk") {
            handleTalkJson(json);
            return;
        }
        if (json.Function === "visual") {
            handleVisualJson(json);
            return;
        }
    }

    //## page layout ###############################################################################

    function positionBackground() {
        data.background.style.position = "fixed";
        data.background.style.width = utils.toPx(window.innerWidth);
        data.background.style.height = utils.toPx(window.innerHeight);
    }

    function positionLogin() {
        var width = 500;
        var height = 230;
        data.login.container.style.position = "fixed";
        data.login.container.style.width = utils.toPx(width);
        data.login.container.style.height = utils.toPx(height);
        data.login.container.style.top = utils.toPx((window.innerHeight - height) / 2);
        data.login.container.style.left = utils.toPx((window.innerWidth - width) / 2);
        data.login.controls.style.marginTop = utils.toPx((height - data.login.controls.clientHeight) / 2);
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
        positionBackground();
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
        resourceBuffer.addSvg("login/logo", "images/logo.svg", "");
        resourceBuffer.addSvg("hud/talk", "images/hud/talk.svg", "");
    }

    function initializeGameHud() {
        data.game.hudTalk = new gameHudImageElement(resourceBuffer.get("hud/talk"));
        data.game.hudTalk.onClick = onGameHudTalkClick;
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
        setInterval(updateGameUiData, 1000 / data.game.fps);
        setInterval(drawGameCanvas, 1000 / data.game.fps);
        data.game.canvas.onclick = onGameCanvasClick;
    }

    function onWindowLoad() {
        data.background = document.getElementById("background");
        data.footer = document.getElementById("footer");
        preloadResources();
        initializeLogin();
        initializeGame();
        // [Randy 08/10/2010] REMARK: Attaching to the canvas doesn't seem to work.
        document.onkeyup = onCanvasKeyup;
        setStateToLogin();
        initializeWebSocket();
        layoutPage();
        layoutGameHud();
        data.onWindowLoaded = true;
    }

    function onWindowResize() {
        if (!data.onWindowLoaded) {
            onWindowLoad();
        }
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
            test("replaceAll", ("testtest".replaceAll("test", "ba") === "baba"));
            test("toPx", (utils.toPx(10) === "10px"));
            test("fromPx", (utils.fromPx("10px") === 10));
            test("lerp1", (utils.lerp(0.0, 10.0, 0.5) === 5.0));
            test("lerp2", (utils.lerp(10.0, 30.0, 0.5) === 20.0));
            test("lerp3", (utils.lerp(10.0, 30.0, 0.0) === 10.0));
            test("lerp4", (utils.lerp(10.0, 30.0, 1.0) === 30.0));
            test("removeTrailingEnter", (utils.removeTrailingEnter("test\n") === "test"));
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
