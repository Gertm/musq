//## utility functions #########################################################################

function toTileIndex(x, y) {
    return "" + x + "," + y;
}

function jsonTileToTile(jsontile) {
    var tile = {};
    tile.properties = jsontile.Properties;
    tile.images = [];
    for (var iImage = 0; iImage < jsontile.Images.length; ++iImage) {
        var url = jsontile.Images[iImage];
        var image = loadImage(url);
        if (image) {
            tile.images.push(image);
        }
    }
    return tile;
}

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
gameHudImageElement.prototype = new gameHudElement();

//## canvas elements ###########################################################################

function gameEntity() {
    this.moveAnimation = new moveAnimation();
    this.messages = [];
}

//## global data ###############################################################################

game = {};
game.fps = 30;
game.viewPortCenter = new moveAnimation();
game.lastUpdateTime = now();
game.talking = false;
game.showtalkhistory = false;
game.talkhistory = [];
game.logicalToVisualFactor = 64.0;
game.hudelements = {};
game.entities = {};

//## utilities that depend on the global data ##################################################

function logicalToVisual(xy) {
    var viewPortCenter = game.viewPortCenter.curr;
    var x = Math.round(game.canvas.width / 2 + (xy.x - viewPortCenter.x) * game.logicalToVisualFactor);
    var y = Math.round(game.canvas.height / 2 - (xy.y - viewPortCenter.y) * game.logicalToVisualFactor);
    return new vecMath.vector2d(x, y);
}

function visualToLogic(xy) {
    var viewPortCenter = game.viewPortCenter.curr;
    var x = Math.round((xy.x - game.canvas.width / 2) / game.logicalToVisualFactor + viewPortCenter.x);
    var y = Math.round((xy.y - game.canvas.height / 2) * -1 / game.logicalToVisualFactor + viewPortCenter.y);
    return new vecMath.vector2d(x, y);
}

function getLogicalViewPort() {
    return {
        topLeft: visualToLogic(new vecMath.vector2d(0.0, 0.0)),
        bottomRight: visualToLogic(new vecMath.vector2d(game.canvas.width - 1, game.canvas.height - 1))
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

//## drawing ###################################################################################

function drawGameBackground(cxt) {
    cxt.fillStyle = "#000000";
    cxt.fillRect(0, 0, game.canvas.width, game.canvas.height);
}

function drawGameTile(cxt, x, y) {
    var tile = game.area.tiles[toTileIndex(x, y)];
    if (!tile) {
        return;
    }
    var uipt = logicalToVisual({ x: x, y: y });
    // TODO: Set scale depending on game.logicalToVisualFactor and image.width.
    for (var iImage = 0; iImage < tile.images.length; ++iImage) {
        var image = tile.images[iImage];
        cxt.drawImage(image, uipt.x - image.width / 2, uipt.y - image.height / 2);
    }
}

function drawGameArea(cxt) {
    if (!game.area || !game.area.tiles) {
        return;
    }
    var viewPort = getLogicalViewPort();
    for (var x = viewPort.topLeft.x - 1; x < viewPort.bottomRight.x + 1; x++) {
        for (var y = viewPort.bottomRight.y - 1; y < viewPort.topLeft.y + 1; y++) {
            drawGameTile(cxt, x, y);
        }
    }
}

function drawGameMoveTarget(cxt) {
    if (data.playerName === "" || !game.entities[data.playerName]) {
        return;
    }
    cxt.fillStyle = "#FF0000";
    var pt = logicalToVisual(game.entities[data.playerName].moveAnimation.dst);
    cxt.fillRect(pt.x - 2, pt.y - 2, 4, 4);
}

function drawTalk(cxt, entityPtLogic, message) {
    var entityPtUi = logicalToVisual(entityPtLogic);
    var txtoffset = { x: game.logicalToVisualFactor * 0.3, y: -game.logicalToVisualFactor * 0.4 };
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
    for (var eI in game.entities) {
        var e = game.entities[eI];
        if (e.image) {
            drawImageAroundLogic(cxt, e.image, e.moveAnimation.curr);
        }
        if (e.messages.length !== 0) {
            drawTalk(cxt, e.moveAnimation.curr, e.messages[0]);
        }
    }
}

function drawTalkHistory(cxt) {
    if (!game.showtalkhistory) {
        return;
    }
    var canvasrc = { x: 0, y: 0, width: game.canvas.width, height: game.canvas.height };
    var rc = expandRc(canvasrc, -50, -50);
    cxt.save();
    cxt.fillStyle = "rgba(0, 0, 0, 0.5)";
    cxt.fillRect(rc.x, rc.y, rc.width, rc.height);
    var textlineheight = 12;
    var textseparator = 5;
    var textfulllineheight = textlineheight + textseparator;
    var textfullheight = textfulllineheight * game.talkhistory.length;
    var ylimitmin = rc.y + 10;
    var ylimitmax = rc.y + rc.height - 10;
    cxt.font = textlineheight + "px SmackAttackBB";
    cxt.textAlign = "left";
    cxt.textBaseline = "top";
    var yoffset = ylimitmin;
    if (yoffset + textfullheight > ylimitmax) {
        yoffset = ylimitmax - textfullheight;
    }
    game.talkhistory.forEach(function (e, ei, a) {
                                 cxt.fillStyle = "#ffffff";
                                 if (yoffset >= ylimitmin) {
                                     cxt.fillText(e.From + ": " + e.Msg, rc.x + 10, yoffset);
                                 }
                                 yoffset += textfulllineheight;
                             });
    cxt.restore();
}

function drawGameHud(cxt) {
    for (var eI in game.hudelements) {
        var e = game.hudelements[eI];
        e.draw(cxt);
    }
    drawTalkHistory(cxt);
}

function drawGameCanvas() {
    if (data.state !== "game") {
        return;
    }
    var cxt = game.canvas.getContext("2d");
    drawGameBackground(cxt);
    drawGameArea(cxt);
    drawGameMoveTarget(cxt);
    drawGameEntities(cxt);
    drawGameHud(cxt);
}

//## updating ##################################################################################

function updateGameUiData() {
    if (data.state !== "game") {
        return;
    }
    var newUpdateTime = now();
    var timeDiffInMs = newUpdateTime - game.lastUpdateTime;
    game.viewPortCenter.update(timeDiffInMs);
    for (var eI in game.entities) {
        var e = game.entities[eI];
        e.moveAnimation.update(timeDiffInMs);
    }
    game.lastUpdateTime = newUpdateTime;
}

//## message handler helpers ###################################################################

function startTalking() {
    game.talking = true;
    game.talkedit.style.display = "block";
    // [Randy 12/11/2010] PATCH: ClientHeight only seems to be filled if the element
    // is really visible.
    positionTalkEdit();
    game.talkedit.focus();
}

function sendTalkMessage() {
    var message = removeTrailingEnter(game.talkedit.value);
    if (message !== "") {
        wsSend({
                   "Function": "talk",
                   "Params": {
                       "Message": message
                   }
               });
    }
    game.talkedit.value = "";
}

function stopTalking() {
    game.talking = false;
    game.talkedit.style.display = "none";
}

function clearTalkMessage(playerName) {
    var player = game.entities[playerName];
    if (!player)
        return;
    if (player.messages.length === 0)
        return;
    player.messages = player.messages.slice(1);
}

function ensurePlayerIsWithinViewPort() {
    var playerDst = game.entities[data.playerName].moveAnimation.dst;
    var viewPort = getLogicalViewPort();
    var border = 3;
    var newViewPortCenter = new vecMath.vector2d(game.viewPortCenter.dst.x, game.viewPortCenter.dst.y);
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
    game.viewPortCenter.setDestination(newViewPortCenter, 1.0);
}

function requestTalkHistory() {
    wsSend({
               "Function": "chatHistory",
               "Params": {}
           });
}

function swapShowTalkHistory() {
    game.showtalkhistory = !game.showtalkhistory;
}

function limitTalkHistory() {
    if (game.talkhistory.length > 20) {
        game.talkhistory = game.talkhistory.slice(game.talkhistory.length - 20);
    }
}

function handleAreaJson(json) {
    game.area = {};
    game.area.width = json.Params.Width;
    game.area.height = json.Params.Height;
    game.area.tiles = {};
    var x;
    var y;
    var defaulttile = jsonTileToTile(json.Params.DefaultTile);
    for (x = 0; x < game.area.width; x++) {
        for (y = 0; y < game.area.height; y++) {
            game.area.tiles[toTileIndex(x, y)] = defaulttile;
        }
    }
    var bordertile = jsonTileToTile(json.Params.BorderTile);
    for (x = -1; x <= game.area.width; x++) {
        game.area.tiles[toTileIndex(x, -1)] = bordertile;
        game.area.tiles[toTileIndex(x, game.area.height)] = bordertile;
    }
    for (y = -1; y <= game.area.height; y++) {
        game.area.tiles[toTileIndex(-1, y)] = bordertile;
        game.area.tiles[toTileIndex(game.area.width, y)] = bordertile;
    }
    for (var iTile = 0; iTile < json.Params.Tiles.length; ++iTile) {
        var jsontile = json.Params.Tiles[iTile];
        var tile = jsonTileToTile(jsontile);
        game.area.tiles[toTileIndex(jsontile.X, jsontile.Y)] = tile;
    }
    // [Randy 05/01/2011] REMARK: New 'enter' messages will be sent.
    game.entities = {};
}

function handleEnterJson(json) {
    var player = new gameEntity();
    player.image = convertSvgs(json.Params.Images);
    game.entities[json.Params.Name] = player;
    var pos = new vecMath.vector2d(parseInt(json.Params.X, 10), parseInt(json.Params.Y, 10));
    player.moveAnimation.initialize(pos);
    if (json.Params.Name === data.playerName) {
        game.viewPortCenter.initialize(pos);
    }
}

function handleLeaveJson(json) {
    delete game.entities[json.Params.Name];
}

function handleJumpJson(json) {
    var player = game.entities[json.Params.Name];
    if (!player) {
        return;
    }
    var pos = new vecMath.vector2d(parseInt(json.Params.X, 10), parseInt(json.Params.Y, 10));
    player.moveAnimation.initialize(pos);
    if (json.Params.Name === data.playerName) {
        ensurePlayerIsWithinViewPort();
    }
}

function handleMoveJson(json) {
    var newDestination = new vecMath.vector2d(parseInt(json.Params.X, 10), parseInt(json.Params.Y, 10));
    var player = game.entities[json.Params.Name];
    if (!player) {
        return;
    }
    player.moveAnimation.setDestination(newDestination, 1.0);
    if (json.Params.Name === data.playerName) {
        ensurePlayerIsWithinViewPort();
    }
}

function handleTalkJson(json) {
    game.talkhistory.push({ From: json.Params.Name, Msg: json.Params.Message });
    limitTalkHistory();
    var player = game.entities[json.Params.Name];
    if (!player) {
        return;
    }
    player.messages.push(json.Params.Message);
    var timeout = 4000 + json.Params.Message.length * 100;
    setTimeout(function () { clearTalkMessage(json.Params.Name); }, timeout);
}

function handleChatHistoryJson(json) {
    game.talkhistory = json.Params.Lines;
    limitTalkHistory();
}

//## message handlers ##########################################################################

function onGameCanvasClick(evt) {
    if (data.state !== "game") {
        return;
    }
    var offsetX = onclickOffset(evt, "X", game.canvas);
    var offsetY = onclickOffset(evt, "Y", game.canvas);
    var pt = new vecMath.vector2d(offsetX, offsetY);
    for (var eI in game.hudelements) {
        var e = game.hudelements[eI];
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

function onGameHudTalkClick() {
    if (!game.talking) {
        startTalking();
    } else {
        stopTalking();
    }
}

function onGameHudTalkHistoryClick() {
    swapShowTalkHistory();
}

function onGameKeyUp(keyunicode) {
    if (keyunicode == 72 /* h */) {
        if (!game.talking) {
            swapShowTalkHistory();
        }
        return;
    }
    if (keyunicode == 84 /* t */) {
        if (!game.talking) {
            startTalking();
        }
        return;
    }
    if (keyunicode == 13 /* enter */) {
        if (game.talking) {
            sendTalkMessage();
            stopTalking();
        }
        return;
    }
}

//## page layout ###############################################################################

function positionGameCanvas() {
    var xPadding = 40;
    var yPadding = 20;
    game.canvas.style.position = "fixed";
    var newWidth = window.innerWidth - xPadding * 2;
    game.canvas.setAttribute("width", toPx(newWidth));
    var newHeight = window.innerHeight - yPadding * 2 - data.footer.clientHeight;
    game.canvas.setAttribute("height", toPx(newHeight));
    game.canvas.style.top = toPx(yPadding);
    game.canvas.style.left = toPx(xPadding);
}

function positionTalkEdit() {
    game.talkedit.style.position = "fixed";
    game.talkedit.style.top = toPx(fromPx(game.canvas.style.top) + game.canvas.height - game.talkedit.clientHeight);
    game.talkedit.style.left = game.canvas.style.left;
    game.talkedit.style.width = toPx(game.canvas.width);
}

function layoutGameHud() {
    // [Randy 13/11/2010] TODO: Implement a vertical stack layout (so these values aren't hardcoded).
    game.hudelements.talk.x = 20;
    game.hudelements.talk.y = 20;
    game.hudelements.talkhistory.x = 20;
    game.hudelements.talkhistory.y = 50;
}

//## initialization ############################################################################

function initializeGameHud() {
    game.hudelements.talk = new gameHudImageElement(convertSvg("images/hud/talk.svg"));
    game.hudelements.talk.onClick = onGameHudTalkClick;
    game.hudelements.talkhistory = new gameHudImageElement(convertSvg("images/hud/talkhistory.svg"));
    game.hudelements.talkhistory.onClick = onGameHudTalkHistoryClick;
}

function initializeGame() {
    game.container = document.getElementById("gamecontainer");
    game.canvas = document.getElementById("gamecanvas");
    game.talkedit = document.getElementById("gametalkedit");
    initializeGameHud();
    setInterval(updateGameUiData, 1000 / game.fps);
    setInterval(drawGameCanvas, 1000 / game.fps);
    game.canvas.onclick = onGameCanvasClick;
}
