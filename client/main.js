function setFullWindow(obj, margin) {
    var mainWidth = window.innerWidth;
    var mainHeight = window.innerHeight;
    var width = mainWidth - 2 * margin;
    var height = mainHeight - 2 * margin;
    obj.style.position = "fixed";
    obj.style.width = width+"px";
    obj.style.height = height+"px";
    obj.style.top = margin+"px";
    obj.style.left = margin+"px";
}

function centerWindow(obj) {
    var mainWidth = window.innerWidth;
    var mainHeight = window.innerHeight;
    var width = obj.width;
    var height = obj.height;
    var top = (mainHeight - height) / 2;
    var left = (mainWidth - width) / 2;
    obj.style.position = "fixed";
    obj.style.top = top+"px";
    obj.style.left = left+"px";
}

function drawImageOnLoad(cxt, url, x, y) {
    var img = new Image();
    img.onload = function() {
	cxt.fillStyle = "#0000FF";
	cxt.drawImage(img, x, y);
    };
    img.src = url;
}

function drawCanvas() {
    var canvas = document.getElementById("maincanvas");
    var cxt = canvas.getContext("2d");
    cxt.fillStyle = "#00FF00";
    cxt.fillRect(0, 0, canvas.width - 1, canvas.height - 1);
    drawImageOnLoad(cxt, "images/faces/human/human01.svg", 250, 140);
}

window.onload = window.onresize = function() {
    drawCanvas();
};
