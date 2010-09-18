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

function drawCanvas() {
    var canvas = document.getElementById("maincanvas");
    var cxt = canvas.getContext("2d");
    cxt.fillStyle = "#00FF00";
    cxt.fillRect(0, 0, canvas.width - 1, canvas.height - 1);    
}

window.onload = window.onresize = function() {
    setFullWindow(document.getElementById("background_layer0"), 0);
    setFullWindow(document.getElementById("background_layer1"), 0);
    setFullWindow(document.getElementById("maincanvas"), 50);
    drawCanvas();
};
