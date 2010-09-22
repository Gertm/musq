var musq = {

    _private: {

	helpers: {
	    toPx: function(i) {
		return i + "px";
	    }
	},

	resourceBuffer: function () {
	    var container = {};
	    return {
		addImage: function (key, url) {
		    var image = new Image();
		    image.src = url;
		    container[key] = image;
		},
		remove: function (key) {
		    container[key] = undefined;
		},
		get: function (key) {
		    return container[key];
		}
	    };
	} (),

	footerHeight: 20,

	positionCanvas: function() {
	    var toPx = musq._private.helpers.toPx;
	    var canvas = document.getElementById("maincanvas");
	    var footer = document.getElementById("footer");
	    var xPadding = 40;
	    var yPadding = 20;
	    canvas.style.position = "fixed";
	    var newWidth = window.innerWidth - xPadding * 2;
	    canvas.setAttribute("width", toPx(newWidth));
	    var newHeight = window.innerHeight - yPadding * 2 - musq._private.footerHeight;
	    canvas.setAttribute("height", toPx(newHeight));
	    canvas.style.top = toPx(yPadding);
	    canvas.style.left = toPx(xPadding);
	},

	positionFooter: function() {
	    var toPx = musq._private.helpers.toPx;
	    var footer = document.getElementById("footer");
	    footer.style.position = "fixed";
	    footer.style.width = toPx(window.innerWidth);
	    footer.style.top = toPx(window.innerHeight - musq._private.footerHeight);
	    footer.style.left = toPx(0);
	},

	drawCanvas: function() {
	    var canvas = document.getElementById("maincanvas");
	    var cxt = canvas.getContext("2d");
	    cxt.fillStyle = "#FFFFFF";
	    cxt.fillRect(0, 0, canvas.width - 1, canvas.height - 1);
	    cxt.drawImage(musq._private.resourceBuffer.get("human01"), 250, 140);
	},

	onWindowResize: function() {
	    musq._private.positionCanvas();
	    musq._private.positionFooter();
	    musq._private.drawCanvas();
	},

	onWindowLoad: function() {
	    musq._private.resourceBuffer.addImage("human01", "images/faces/human/human01.svg");
	    musq._private.onWindowResize();
	    setInterval(musq._private.drawCanvas, 1000);
	}

    }

};

window.onload = musq._private.onWindowLoad;
window.onresize = musq._private.onWindowResize;
