var musq = function() {

    //##############################################################################################

    var utils = function() {

	function toPx(i) {
	    return i + "px";
	}

	function lerp(i1, i2, f) {
	    return i1 + (i2 - i1) * f;
	}

	return {
	    toPx: toPx,
	    lerp: lerp
	};
	
    }();

    //##############################################################################################

    var vecMath = function() {

	function Vector2d(x, y) {
	    return {
		x: x,
		y: y
	    };
	}

	return {
	    Vector2d: Vector2d  
	};

    }();

    //##############################################################################################

    var data = {};

    // These are all in 'logical' coordinates, not in UI pixels.
    data.viewPortCenter = vecMath.Vector2d(0.0, 0.0);
    data.playerUiSide = vecMath.Vector2d(0.0, 0.0);
    data.playerLogicSide = vecMath.Vector2d(10.0, 10.0);

    data.playerSpeed = 1.0;

    data.now = function() {
	return (new Date()).getTime();
    };

    data.lastUpdateTime = data.now();

    //##############################################################################################

    var communication = function() {

	if (window.WebSocket) {

	    var ws = new WebSocket("ws://"+musq_websocket_url+"/service");

	    var onOpen = function() {
		alert("WebSocket opened.");
		ws.send("hello");
	    };
	    ws.onopen = onOpen;

	    var onClose = function() {
		alert("WebSocket closed.");
	    };
	    ws.onclose = onClose;

	    var onReceive = function(data) {
		alert("Received " + data);
	    };
	    ws.onmessage = function(evt) {
		onReceive(evt.data);
	    };

	    function send(data) {
		ws.send(data);
	    }

	    return {
		send: send,
		onOpen: onOpen,
		onClose: onClose,
		onReceive: onReceive
	    };
	    
	} else {

	    alert("Sorry, your browser does not support websockets.");
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

	var footerHeight = 20;

	function positionCanvas() {
	    var canvas = document.getElementById("maincanvas");
	    var footer = document.getElementById("footer");
	    var xPadding = 40;
	    var yPadding = 20;
	    canvas.style.position = "fixed";
	    var newWidth = window.innerWidth - xPadding * 2;
	    canvas.setAttribute("width", utils.toPx(newWidth));
	    var newHeight = window.innerHeight - yPadding * 2 - footerHeight;
	    canvas.setAttribute("height", utils.toPx(newHeight));
	    canvas.style.top = utils.toPx(yPadding);
	    canvas.style.left = utils.toPx(xPadding);
	}

	function positionFooter() {
	    var footer = document.getElementById("footer");
	    footer.style.position = "fixed";
	    footer.style.width = utils.toPx(window.innerWidth);
	    footer.style.top = utils.toPx(window.innerHeight - footerHeight);
	    footer.style.left = utils.toPx(0);
	}

	var logicalToVisualFactor = 50.0;

	function logicalToVisual(xy) {
	    var canvas = document.getElementById("maincanvas");
	    return vecMath.Vector2d(
		Math.round(canvas.width / 2 + xy.x * logicalToVisualFactor),
		Math.round(canvas.height / 2 - xy.y * logicalToVisualFactor)
	    );
	}

	function visualToLogic(xy) {
	    var canvas = document.getElementById("maincanvas");
	    return vecMath.Vector2d(
		(xy.x - canvas.width / 2) / logicalToVisualFactor,
		(xy.y - canvas.height / 2) * -1 / logicalToVisualFactor
	    );
	}

	function drawSvgAround(cxt, key, pt) {
	    // TODO: Find a way of determining the width/height of a svg.
	    var svgWidth = 48;
	    var svgHeight = 48;
	    cxt.drawSvg(resourceBuffer.get(key), pt.x - svgWidth / 2, pt.y - svgHeight / 2);
	}

	function drawCanvas() {
	    var canvas = document.getElementById("maincanvas");
	    var cxt = canvas.getContext("2d");
	    cxt.fillStyle = "#FFFFFF";
	    cxt.fillRect(0, 0, canvas.width - 1, canvas.height - 1);
	    cxt.fillStyle = "#FF0000";
	    var playerLogicalVisual = logicalToVisual(data.playerLogicSide);
	    cxt.fillRect(playerLogicalVisual.x - 2, playerLogicalVisual.y -2, 4, 4);
	    var playerUiVisual = logicalToVisual(data.playerUiSide);
	    drawSvgAround(cxt, "human01", playerUiVisual);
	}

	function updateUiData() {
	    var newUpdateTime = data.now();
	    var distance = data.playerSpeed * (newUpdateTime - data.lastUpdateTime) * 0.001;
	    if (data.playerUiSide.x < data.playerLogicSide.x)
		data.playerUiSide.x += distance;
	    if (data.playerUiSide.x > data.playerLogicSide.x)
		data.playerUiSide.x -= distance;
	    if (data.playerUiSide.y < data.playerLogicSide.y)
		data.playerUiSide.y += distance;
	    if (data.playerUiSide.y > data.playerLogicSide.y)
		data.playerUiSide.y -= distance;
	    data.lastUpdateTime = newUpdateTime;
	}

	function setRandomPlayerLogicalSide() {
	    var canvas = document.getElementById("maincanvas");
	    data.playerLogicSide = visualToLogic(vecMath.Vector2d(
						     utils.lerp(0, canvas.width - 1, Math.random()),
						     utils.lerp(0, canvas.height - 1, Math.random())
						 ));
	}

	function onWindowResize() {
	    positionCanvas();
	    positionFooter();
	    drawCanvas();
	}

	function onWindowLoad() {
	    resourceBuffer.addXml("human01", "images/faces/human/human01.svg");
	    onWindowResize();
	    var fps = 30;
	    setInterval(updateUiData, 1000 / fps);
	    setInterval(drawCanvas, 1000 / fps);
	    setRandomPlayerLogicalSide();
	    setInterval(setRandomPlayerLogicalSide, 10000);
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
		alert("test failed: " + testString + "!");
	    }
	}

	function runTests() {
	    test("toPx", (utils.toPx(10) == "10px"));
	    test("lerp1", (utils.lerp(0.0, 10.0, 0.5) == 5.0));
	    test("lerp2", (utils.lerp(10.0, 30.0, 0.5) == 20.0));
	    test("lerp3", (utils.lerp(10.0, 30.0, 0.0) == 10.0));
	    test("lerp4", (utils.lerp(10.0, 30.0, 1.0) == 30.0));
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
