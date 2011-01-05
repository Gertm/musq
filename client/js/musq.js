//## global data ###############################################################################

var data = {};
data.onWindowLoaded = false;
data.state = "init";
data.playerName = "";

//## utilities that depend on the global data ##################################################

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

//## message handler helpers ###################################################################

function setStateToLogin() {
    login.container.style.display = "block";
    game.container.style.display = "none";
    createaccount.container.style.display = "none";
    data.state = "login";
    login.username.focus();
}

function setStateToGame() {
    login.container.style.display = "none";
    game.container.style.display = "block";
    createaccount.container.style.display = "none";
    data.state = "game";
}

function setStateToCreateAccount() {
    login.container.style.display = "none";
    game.container.style.display = "none";
    createaccount.container.style.display = "block";
    data.state = "createaccount";
    createaccount.username.focus();
    if (!createaccount.faces.human.male.faces.urls) {
        createAccountInitializeImages();
    }
}

//## message handlers ##########################################################################

function onCanvasKeyup(evt) {
    var keyunicode = onkeyKey(evt);
    //alert(keyunicode);
    if (data.state === "login") {
        onLoginKeyUp(keyunicode);
        return;
    }
    if (data.state === "game") {
        onGameKeyUp(keyunicode);
        return;
    }
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
    if (json.Function === "area") {
        handleAreaJson(json);
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
    if (json.Function === "enter") {
        handleEnterJson(json);
        return;
    }
    if (json.Function === "getFiles") {
        handleGetFilesJson(json);
        return;
    }
    if (json.Function === "jump") {
        handleJumpJson(json);
        return;
    }
    if (json.Function === "keepalive") {
        return;
    }
    if (json.Function === "leave") {
        handleLeaveJson(json);
        return;
    }
    if (json.Function === "login") {
        handleLoginJson(json);
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
}

//## page layout ###############################################################################

function positionBackground() {
    data.background.style.position = "fixed";
    data.background.style.width = toPx(window.innerWidth);
    data.background.style.height = toPx(window.innerHeight);
}

function positionFooter() {
    data.footer.style.position = "fixed";
    data.footer.style.width = toPx(window.innerWidth);
    data.footer.style.top = toPx(window.innerHeight - data.footer.clientHeight);
    data.footer.style.left = toPx(0);
}

function layoutPage() {
    positionBackground();
    positionLogin();
    positionGameCanvas();
    positionFooter();
    positionTalkEdit();
}

//## initialization ############################################################################

function initializeWebSocket() {
    data.ws = new WebSocket("ws://" + musq_websocket_url + "/service");
    data.ws.onopen = onWebSocketOpened;
    data.ws.onclose = onWebSocketClosed;
    data.ws.onmessage = onWebSocketMessage;
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

//##################################################################################################

var runtests = false;
if (!runtests) {
    window.onload = onWindowLoad;
    window.onresize = onWindowResize;
}
else {
    window.onload = testing.runTests;
}
