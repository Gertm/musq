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
			if (offsetAxis)
				return offsetAxis;
			else if (axis == "X")
			return evt.clientX - fromPx(obj.style.left);	    
			else
				return evt.clientY - fromPx(obj.style.top);	    
		}
		
		return {
			toPx: toPx,
			fromPx: fromPx,
			lerp: lerp,
	    onclickOffset: onclickOffset
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

    data.playerSpeed = 1.0;

    data.now = function() {
	return (new Date()).getTime();
    };

    data.lastUpdateTime = data.now();

    //##############################################################################################

    function log(txt) {
	if (console.log)
	    console.log("MUSQ: " + txt);
    }

    //##############################################################################################

    var communication = function() {

	if (window.WebSocket) {

	    var ws = new WebSocket("ws://"+musq_websocket_url+"/service");

	    ws.onopen = function() {
		log("WebSocket opened.");
			ws.send(JSON.stringify({
									   "function": "login",
									   "params": {
										   "username": "Randy",
										   "password": ""
									   }
								   }));
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
		if (json.Function == "move") {
		    data.playerLogicSide = new vecMath.vector2d(parseInt(json.Params.x), parseInt(json.Params.y));
		    return;
		}
	    };

	    function send(obj) {
		ws.send(JSON.stringify(obj));
	    }

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

	var logicalToVisualFactor = 100.0;

	function logicalToVisual(xy) {
	    var canvas = document.getElementById("maincanvas");
	    return new vecMath.vector2d(
		Math.round(canvas.width / 2 + xy.x * logicalToVisualFactor),
		Math.round(canvas.height / 2 - xy.y * logicalToVisualFactor)
	    );
	}

	function visualToLogic(xy) {
	    var canvas = document.getElementById("maincanvas");
	    return new vecMath.vector2d(
		Math.round((xy.x - canvas.width / 2) / logicalToVisualFactor),
		Math.round((xy.y - canvas.height / 2) * -1 / logicalToVisualFactor)
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
	    cxt.fillRect(playerLogicalVisual.x - 2, playerLogicalVisual.y - 2, 4, 4);
	    var playerUiVisual = logicalToVisual(data.playerUiSide);
	    drawSvgAround(cxt, "human01", playerUiVisual);
	}

	function updateUiData() {
	    var vm = vecMath;
	    var newUpdateTime = data.now();
	    var distance = data.playerSpeed * (newUpdateTime - data.lastUpdateTime) * 0.001;
	    var v = vm.subtract(data.playerLogicSide, data.playerUiSide);
	    var vLength = v.length();
	    if (vLength > 0.001) {
		data.playerUiSide = vm.add(data.playerUiSide, vm.scale(v, distance / vLength));
	    }
	    data.lastUpdateTime = newUpdateTime;
	}

	function onCanvasClick(evt) {
	    var canvas = document.getElementById("maincanvas");
	    var offsetX = utils.onclickOffset(evt, "X", canvas);
	    var offsetY = utils.onclickOffset(evt, "Y", canvas);
	    var newPosition = visualToLogic(new vecMath.vector2d(offsetX, offsetY));
	    communication.send({
				   "function": "move",
				   "params": {
				       "x": "" + newPosition.x,
				       "y": "" + newPosition.y
				   }
			       });
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
	    var canvas = document.getElementById("maincanvas");
	    canvas.onclick = onCanvasClick;
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
	    test("fromPx", (utils.toPx("10px") == "10"));
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
