var musq = function() {

    //##############################################################################################

    var utils = function() {

	function toPx(i) {
	    return i + "px";
	}

	return {
	    toPx: toPx  
	};
	
    }();

    //##############################################################################################

    var communication = function() {

	if (window.WebSocket) {

	    var ws = new WebSocket("ws://localhost:8080/service");

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
		var xmlDoc = xmlHttp.responseXML;
		container[key] = xmlDoc;
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

	function drawSvg(cxt, svg, x, y) {
	}

	function drawCanvas() {
	    var canvas = document.getElementById("maincanvas");
	    var cxt = canvas.getContext("2d");
	    cxt.fillStyle = "#FFFFFF";
	    cxt.fillRect(0, 0, canvas.width - 1, canvas.height - 1);
	    //drawSvg(cxt, resourceBuffer.get("human01"), 250, 140);
	    cxt.drawImage(resourceBuffer.get("human01"), 250, 140);
	}

	function onWindowResize() {
	    positionCanvas();
	    positionFooter();
	    drawCanvas();
	}

	function onWindowLoad() {
	    //resourceBuffer.addXml("human01", "images/faces/human/human01.svg");
	    resourceBuffer.addImage("human01", "images/faces/human/human01.svg");
	    onWindowResize();
	    setInterval(drawCanvas, 1000);
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
