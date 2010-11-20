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
