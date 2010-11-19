var musq = function () {

    //## generic utilities that have no dependencies ###############################################

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

    String.prototype.replaceAll = function (oldS, newS) {
        return stringReplaceRecursive(this, oldS, newS);
    };

    function expandRc(rc, xborder, yborder) {
        return {
            x: rc.x - xborder,
            y: rc.y - yborder,
            width: rc.width + 2 * xborder,
            height: rc.height + 2 * yborder
        };
    }

    function pushFront(a, e) {
        a.reverse();
        a.push(e);
        a.reverse();
    }

    function loadImage(url) {
        var image = new Image();
        image.src = url;
        return image;
    }

    function drawIfExists(cxt, image, x, y) {
        if (image) {
            cxt.drawImage(image, x, y);
        }
    }

    function randomElement(array) {
        if (array.length === 0) {
            return undefined;
        }
        return array[Math.round(Math.random() * (array.length - 1))];
    }

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
            return ptInRc(rc, pt);
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
        inherit(this, gameHudElement);
        this.width = image.width;
        this.height = image.height;
        this.draw = function (cxt) {
            cxt.drawImage(image, this.x, this.y);
            // [Randy 06/11/2010] PATCH: Sometimes the image isn't loaded directly and
            // as a result the width and height weren't correctly transferred.
            this.width = image.width;
            this.height = image.height;
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
    data.login.lastUpdateTime = now();
    data.login.scaleFactor = 1.0;
    data.login.scaleDirection = -1.0;
    data.game = {};
    data.game.fps = 30;
    data.game.viewPortCenter = new moveAnimation();
    data.game.lastUpdateTime = now();
    data.game.talking = false;
    data.game.showtalkhistory = false;
    data.game.talkhistory = [];
    data.game.logicalToVisualFactor = 64.0;
    data.game.hudelements = {};
    data.game.entities = {};
    data.createaccount = {};
    data.createaccount.faces = {};
    data.createaccount.faces.buffers = [];
    data.createaccount.faces.human = {};
    data.createaccount.faces.human.male = {};
    data.createaccount.faces.human.male.ears = {};
    data.createaccount.faces.buffers.push(data.createaccount.faces.human.male.ears);
    data.createaccount.faces.human.male.ears.colors = ["#fff0c1", "#785d42"];
    data.createaccount.faces.human.male.faces = {};
    data.createaccount.faces.buffers.push(data.createaccount.faces.human.male.faces);
    data.createaccount.faces.human.male.faces.colors = ["#fff0c1", "#785d42"];
    data.createaccount.faces.human.male.eyes = {};
    data.createaccount.faces.buffers.push(data.createaccount.faces.human.male.eyes);
    data.createaccount.faces.human.male.eyes.colors = ["#000000", "#000044"];
    data.createaccount.faces.human.male.hairs = {};
    data.createaccount.faces.buffers.push(data.createaccount.faces.human.male.hairs);
    data.createaccount.faces.human.male.hairs.colors = ["#140d00"];
    data.createaccount.faces.human.male.mouths = {};
    data.createaccount.faces.buffers.push(data.createaccount.faces.human.male.mouths);
    data.createaccount.faces.human.male.mouths.colors = [];
    data.createaccount.faces.human.male.noses = {};
    data.createaccount.faces.buffers.push(data.createaccount.faces.human.male.noses);
    data.createaccount.faces.human.male.noses.colors = [];
    data.createaccount.faces.requestQueue = [];

    //## utilities that depend on the global data ##################################################

    function convertSvgs(urlsAndColors) {
        var canvas = document.getElementById("svg2pngcanvas");
        var cxt = canvas.getContext("2d");
        cxt.clearRect(0, 0, canvas.width, canvas.height);
        urlsAndColors.forEach(function (e, index, array) {
                                  var xmlHttp = new XMLHttpRequest();
                                  if (xmlHttp.overrideMimeType) {
                                      xmlHttp.overrideMimeType('text/xml');
                                  }
                                  xmlHttp.onreadystatechange = function () {
                                      if (xmlHttp.readyState === 4 && xmlHttp.status === 200) {
                                          var svg = xmlHttp.responseXML.getElementsByTagName("svg")[0];
                                          var width = parseInt(svg.getAttribute("width"), 10);
                                          var height = parseInt(svg.getAttribute("height"), 10);
                                          if (canvas.width !== width || canvas.height !== height) {
                                              canvas.setAttribute("width", toPx(width));
                                              canvas.setAttribute("height", toPx(height));
                                          }
                                          var svgTxt = e.Color ? xmlHttp.responseText.replaceAll("#badf0d", e.Color) : xmlHttp.responseText;
                                          cxt.drawSvg(svgTxt, 0, 0);
                                      }
                                  };
                                  xmlHttp.open("GET", e.Url, false);
                                  xmlHttp.send();
                              });
        return loadImage(canvas.toDataURL());
    }

    function convertSvg(url, color) {
        return convertSvgs([{"Url": url, "Color": color}]);
    }

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

    function drawImageAroundUi(cxt, image, pt) {
        var width = image.width;
        var height = image.height;
        cxt.drawImage(image, pt.x - width / 2, pt.y - height / 2);
    }

    function drawImageAroundLogic(cxt, image, pt) {
        drawImageAroundUi(cxt, image, logicalToVisual(pt));
    }

    function drawImageAtUi(cxt, image, pt) {
        cxt.drawImage(image, pt.x, pt.y);
    }

    function drawImageAtLogic(cxt, image, pt) {
        drawImageAtUi(cxt, image, logicalToVisual(pt));
    }

    function wsSend(obj) {
        var json = JSON.stringify(obj);
        log("Sending " + json + ".");
        data.ws.send(json);
    }

    function sendKeepAliv(obj) {
        wsSend({
                   "Function": "keepalive",
                   "Params": {}
               });
    }

    function requestImageUrls(baseurl, wildcard, targetarray) {
        data.createaccount.faces.requestQueue.push(targetarray);
        wsSend({
                   "Function": "getFiles",
                   "Params": {
                       "BasePath": baseurl,
                       "WildCard": wildcard
                   }
               });
    }

    function toVisualPart(buffer) {
        if (buffer.color) {
            return { "Url": buffer.url, "Color": buffer.color };
        }
        return { "Url": buffer.url };
    }

    //## drawing ###################################################################################

    function drawLoginCanvas() {
        if (data.state !== "login") {
            return;
        }
        var cxt = data.login.canvas.getContext("2d");
        cxt.save();
        cxt.clearRect(0, 0, data.login.canvas.width, data.login.canvas.height);
        var image = data.login.logo;
        var newWidth = image.width * data.login.scaleFactor;
        cxt.translate((image.width - newWidth) / 2.0, 0.0);
        cxt.scale(data.login.scaleFactor, 1.0);
        cxt.drawImage(image, 0.0, 0.0);
        cxt.restore();
    }

    function drawGameBackground(cxt) {
        cxt.fillStyle = "#88FF88";
        cxt.fillRect(0, 0, data.game.canvas.width, data.game.canvas.height);
    }

    function drawGameGrid(cxt) {
        var viewPort = getLogicalViewPort();
        for (var x = viewPort.topLeft.x - 1; x < viewPort.bottomRight.x + 1; x++) {
            for (var y = viewPort.bottomRight.y - 1; y < viewPort.topLeft.y + 1; y++) {
                var uipt = logicalToVisual({ x: x, y: y });
                // TODO: Set scale depending on data.game.logicalToVisualFactor and image.width.
                cxt.drawImage(data.game.defaulttile, uipt.x - data.game.defaulttile.width / 2, uipt.y - data.game.defaulttile.height / 2);
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
        var backrc = expandRc(txtrc, 5, 8);
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
        for (var eI in data.game.entities) {
            var e = data.game.entities[eI];
            if (e.image) {
                drawImageAroundLogic(cxt, e.image, e.moveAnimation.curr);
            }
            if (e.messages.length !== 0) {
                drawTalk(cxt, e.moveAnimation.curr, e.messages[0]);
            }
        }
    }

    function drawTalkHistory(cxt) {
        if (!data.game.showtalkhistory) {
            return;
        }
        var canvasrc = { x: 0, y: 0, width: data.game.canvas.width, height: data.game.canvas.height };
        var rc = expandRc(canvasrc, -50, -50);
        cxt.save();
        cxt.fillStyle = "rgba(0, 0, 0, 0.5)";
        cxt.fillRect(rc.x, rc.y, rc.width, rc.height);
        var textlineheight = 12;
        var textseparator = 5;
        var textfulllineheight = textlineheight + textseparator;
        var textfullheight = textfulllineheight * data.game.talkhistory.length;
        var ylimitmin = rc.y + 10;
        var ylimitmax = rc.y + rc.height - 10;
        cxt.font = textlineheight + "px SmackAttackBB";
        cxt.textAlign = "left";
        cxt.textBaseline = "top";
        var yoffset = ylimitmin;
        if (yoffset + textfullheight > ylimitmax) {
            yoffset = ylimitmax - textfullheight;
        }
        data.game.talkhistory.forEach(function (e, ei, a) {
                                          cxt.fillStyle = "#ffffff";
                                          if (yoffset >= ylimitmin) {
                                              cxt.fillText(e.From + ": " + e.Msg, rc.x + 10, yoffset);
                                          }
                                          yoffset += textfulllineheight;
                                      });
        cxt.restore();
    }

    function drawGameHud(cxt) {
        for (var eI in data.game.hudelements) {
            var e = data.game.hudelements[eI];
            e.draw(cxt);
        }
        drawTalkHistory(cxt);
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

    function drawCreateAccountCanvas() {
        if (data.state !== "createaccount") {
            return;
        }
        var canvas = data.createaccount.canvas;
        var cxt = canvas.getContext("2d");
        cxt.save();
        cxt.clearRect(0, 0, canvas.width, canvas.height);
        cxt.fillStyle = "#ff0000";
        for (var bufferI in data.createaccount.faces.buffers) {
            var buffer = data.createaccount.faces.buffers[bufferI];
            drawIfExists(cxt, buffer.image, 0, 0);            
        }
        cxt.restore();
    }

    //## updating ##################################################################################

    function updateLoginAnimation() {
        if (data.state !== "login") {
            return;
        }
        var newUpdateTime = now();
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
        var newUpdateTime = now();
        var timeDiffInMs = newUpdateTime - data.game.lastUpdateTime;
        data.game.viewPortCenter.update(timeDiffInMs);
        for (var eI in data.game.entities) {
            var e = data.game.entities[eI];
            e.moveAnimation.update(timeDiffInMs);
        }
        data.game.lastUpdateTime = newUpdateTime;
    }

    //## message handler helpers ###################################################################

    function setStateToLogin() {
        data.login.container.style.display = "block";
        data.game.container.style.display = "none";
        data.createaccount.container.style.display = "none";
        data.state = "login";
        data.login.username.focus();
    }

    function setStateToGame() {
        data.login.container.style.display = "none";
        data.game.container.style.display = "block";
        data.createaccount.container.style.display = "none";
        data.state = "game";
    }

    function setStateToCreateAccount() {
        data.login.container.style.display = "none";
        data.game.container.style.display = "none";
        data.createaccount.container.style.display = "block";
        data.state = "createaccount";
        data.createaccount.username.focus();
        if (!data.createaccount.faces.human.male.faces.urls) {
            requestImageUrls("images/faces/human/male/", "face*.svg", data.createaccount.faces.human.male.faces);
            requestImageUrls("images/faces/human/male/", "ears*.svg", data.createaccount.faces.human.male.ears);
            requestImageUrls("images/faces/human/male/", "eyes*.svg", data.createaccount.faces.human.male.eyes);
            requestImageUrls("images/faces/human/male/", "hair*.svg", data.createaccount.faces.human.male.hairs);
            requestImageUrls("images/faces/human/male/", "mouth*.svg", data.createaccount.faces.human.male.mouths);
            requestImageUrls("images/faces/human/male/", "nose*.svg", data.createaccount.faces.human.male.noses);
        }
    }

    function startTalking() {
        data.game.talking = true;
        data.game.talkedit.style.display = "block";
        // [Randy 12/11/2010] PATCH: ClientHeight only seems to be filled if the element
        // is really visible.
        positionTalkEdit();
        data.game.talkedit.focus();
    }

    function sendTalkMessage() {
        var message = removeTrailingEnter(data.game.talkedit.value);
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

    function requestTalkHistory() {
        wsSend({
                   "Function": "chatHistory",
                   "Params": {}
               });
    }

    function swapShowTalkHistory() {
        data.game.showtalkhistory = !data.game.showtalkhistory;
    }

    function handleLoginJson(json) {
        if (json.Params.Success === "true") {
            data.playerName = data.login.username.value;
            data.login.username.style.backgroundColor = "#FFFFFF";
            data.login.password.style.backgroundColor = "#FFFFFF";
            data.game.entities = {};
            setStateToGame();
            requestTalkHistory();
        } else {
            setLoginIncorrect();
        }
    }

    function handleVisualJson(json) {
        var player = new gameEntity();
        player.image = convertSvgs(json.Params.Images);
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
        data.game.talkhistory.push({ From: json.Params.Name, Msg: json.Params.Message });
        data.game.talkhistory.length = Math.min(data.game.talkhistory.length, 20);
        var player = data.game.entities[json.Params.Name];
        if (!player) {
            return;
        }
        player.messages.push(json.Params.Message);
        var timeout = 4000 + json.Params.Message.length * 100;
        setTimeout(function () { clearTalkMessage(json.Params.Name); }, timeout);
    }

    function handleQuitJson(json) {
        delete data.game.entities[json.Params.Name];
    }

    function handleChatHistoryJson(json) {
        data.game.talkhistory = json.Params.Lines;
    }

    function handleCreateAccountJson(json) {
        if (json.Params.Success === "true") {
            setStateToLogin();
        } else {
            alert("Account creation failed: " + json.Params.Reason);
        }
    }

    function handleGetFilesJson(json) {
        if (data.createaccount.faces.requestQueue.length === 0) {
            log("Received unexpected getFiles.");
            return;
        }
        var buffer = data.createaccount.faces.requestQueue[0];
        buffer.urls = json.Params.Images;
        buffer.url = randomElement(buffer.urls);
        buffer.color = randomElement(buffer.colors);
        buffer.image = convertSvg(buffer.url, buffer.color);
        data.createaccount.faces.requestQueue = data.createaccount.faces.requestQueue.slice(1);
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

    function onCreateAccountButton() {
        if (data.createaccount.password1.value !== data.createaccount.password2.value) {
            alert("Passwords don't match!");
            return;
        }
        wsSend({
                   "Function": "createAccount",
                   "Params":
                   {
                       "Username": data.createaccount.username.value,
                       "Password": data.createaccount.password1.value,
                       "Email": data.createaccount.email.value,
                       "Images": data.createaccount.faces.buffers.map(toVisualPart)
                   }
               });
    }

    function onGameCanvasClick(evt) {
        if (data.state !== "game") {
            return;
        }
        var offsetX = onclickOffset(evt, "X", data.game.canvas);
        var offsetY = onclickOffset(evt, "Y", data.game.canvas);
        var pt = new vecMath.vector2d(offsetX, offsetY);
        for (var eI in data.game.hudelements) {
            var e = data.game.hudelements[eI];
            if (e.onMouseClick(pt)) {
                return;
            }
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
        var keyunicode = onkeyKey(evt);
        //alert(keyunicode);
        if (data.state === "login") {
            if (keyunicode == 13 /* enter */) {
                onLoginButton();
                return;
            }
            return;
        }
        if (data.state === "game") {
            if (keyunicode == 72 /* h */) {
                if (!data.game.talking) {
                    swapShowTalkHistory();
                }
                return;
            }
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

    function onGameHudTalkHistoryClick() {
        swapShowTalkHistory();
    }

    function onWebSocketOpened() {
        log("WebSocket opened.");
        setInterval(sendKeepAliv, 30000);
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
        if (json.Function === "quit") {
            handleQuitJson(json);
            return;
        }
        if (json.Function === "chatHistory") {
            handleChatHistoryJson(json);
            return;
        }
        if (json.Function === "createAccount") {
            handleCreateAccountJson(json);
            return;
        }
        if (json.Function === "getFiles") {
            handleGetFilesJson(json);
            return;
        }
    }

    //## page layout ###############################################################################

    function positionBackground() {
        data.background.style.position = "fixed";
        data.background.style.width = toPx(window.innerWidth);
        data.background.style.height = toPx(window.innerHeight);
    }

    function positionLogin() {
        var width = 500;
        var height = 230;
        data.login.container.style.position = "fixed";
        data.login.container.style.width = toPx(width);
        data.login.container.style.height = toPx(height);
        data.login.container.style.top = toPx((window.innerHeight - height) / 2);
        data.login.container.style.left = toPx((window.innerWidth - width) / 2);
        data.login.controls.style.marginTop = toPx((height - data.login.controls.clientHeight) / 2);
    }

    function positionGameCanvas() {
        var xPadding = 40;
        var yPadding = 20;
        data.game.canvas.style.position = "fixed";
        var newWidth = window.innerWidth - xPadding * 2;
        data.game.canvas.setAttribute("width", toPx(newWidth));
        var newHeight = window.innerHeight - yPadding * 2 - data.footer.clientHeight;
        data.game.canvas.setAttribute("height", toPx(newHeight));
        data.game.canvas.style.top = toPx(yPadding);
        data.game.canvas.style.left = toPx(xPadding);
    }

    function positionFooter() {
        data.footer.style.position = "fixed";
        data.footer.style.width = toPx(window.innerWidth);
        data.footer.style.top = toPx(window.innerHeight - data.footer.clientHeight);
        data.footer.style.left = toPx(0);
    }

    function positionTalkEdit() {
        data.game.talkedit.style.position = "fixed";
        data.game.talkedit.style.top = toPx(fromPx(data.game.canvas.style.top) + data.game.canvas.height - data.game.talkedit.clientHeight);
        data.game.talkedit.style.left = data.game.canvas.style.left;
        data.game.talkedit.style.width = toPx(data.game.canvas.width);
    }

    function layoutPage() {
        positionBackground();
        positionLogin();
        positionGameCanvas();
        positionFooter();
        positionTalkEdit();
    }

    function layoutGameHud() {
        // [Randy 13/11/2010] TODO: Implement a vertical stack layout (so these values aren't hardcoded).
        data.game.hudelements.talk.x = 20;
        data.game.hudelements.talk.y = 20;
        data.game.hudelements.talkhistory.x = 20;
        data.game.hudelements.talkhistory.y = 50;
    }

    //## initialization ############################################################################

    function initializeGameHud() {
        data.game.hudelements.talk = new gameHudImageElement(convertSvg("images/hud/talk.svg", ""));
        data.game.hudelements.talk.onClick = onGameHudTalkClick;
        data.game.hudelements.talkhistory = new gameHudImageElement(convertSvg("images/hud/talkhistory.svg", ""));
        data.game.hudelements.talkhistory.onClick = onGameHudTalkHistoryClick;
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
        data.login.logo = convertSvg("images/logo.svg", "");
        setInterval(updateLoginAnimation, 30);
        setInterval(drawLoginCanvas, 30);
    }

    function initializeGame() {
        data.game.container = document.getElementById("gamecontainer");
        data.game.canvas = document.getElementById("gamecanvas");
        data.game.talkedit = document.getElementById("gametalkedit");
        data.game.defaulttile = loadImage("images/tiles/surfaces/earth01.png");
        initializeGameHud();
        setInterval(updateGameUiData, 1000 / data.game.fps);
        setInterval(drawGameCanvas, 1000 / data.game.fps);
        data.game.canvas.onclick = onGameCanvasClick;
    }

    function initializeCreateAccount() {
        data.createaccount.container = document.getElementById("createaccountcontainer");
        data.createaccount.username = document.getElementById("createaccountusername");
        data.createaccount.button = document.getElementById("createaccountbutton");
        data.createaccount.button.onclick = onCreateAccountButton;
        data.createaccount.password1 = document.getElementById("createaccountpassword1");
        data.createaccount.password2 = document.getElementById("createaccountpassword2");
        data.createaccount.email = document.getElementById("createaccountemail");
        data.createaccount.canvas = document.getElementById("createaccountcanvas");
        setInterval(drawCreateAccountCanvas, 500);
    }

    function onWindowLoad() {
        data.background = document.getElementById("background");
        data.footer = document.getElementById("footer");
        initializeLogin();
        initializeGame();
        initializeCreateAccount();
        // [Randy 08/10/2010] REMARK: Attaching to the canvas doesn't seem to work.
        document.onkeyup = onCanvasKeyup;
        setStateToLogin();
        initializeWebSocket();
        layoutPage();
        layoutGameHud();
        data.onWindowLoaded = true;
        // [Randy 12/11/2010] PATCH: ClientHeight of elements doesn't seem to be calculated yet.
        // So the layout must happen again when they are.
        setTimeout(layoutPage, 500);
    }

    function onWindowResize() {
        if (!data.onWindowLoaded) {
            return;
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
            test("toPx", (toPx(10) === "10px"));
            test("fromPx", (fromPx("10px") === 10));
            test("lerp1", (lerp(0.0, 10.0, 0.5) === 5.0));
            test("lerp2", (lerp(10.0, 30.0, 0.5) === 20.0));
            test("lerp3", (lerp(10.0, 30.0, 0.0) === 10.0));
            test("lerp4", (lerp(10.0, 30.0, 1.0) === 30.0));
            test("removeTrailingEnter", (removeTrailingEnter("test\n") === "test"));
            test("ptInRc1", (ptInRc({ x: 50, y: 50, width: 50, height: 50 }, { x: 60, y: 60 })));
            test("ptInRc2", (!ptInRc({ x: 50, y: 50, width: 50, height: 50 }, { x: 40, y: 60 })));
            test("pushFront", function () { var a = []; pushFront(a, 0); pushFront(a, 1); pushFront(a, 2); return a === [2, 1, 0]; });
        }

        return {
            runTests: runTests
        };

    } ();

    return {
        onWindowLoad: onWindowLoad,
        onWindowResize: onWindowResize,
        setStateToCreateAccount: setStateToCreateAccount,
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
