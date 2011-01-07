//## global data ###############################################################################

login = {};
login.lastUpdateTime = now();
login.scaleFactor = 1.0;
login.scaleDirection = -1.0;

//## drawing ###################################################################################

function drawLoginCanvas() {
    if (data.state !== "login") {
        return;
    }
    var cxt = login.canvas.getContext("2d");
    cxt.save();
    cxt.clearRect(0, 0, login.canvas.width, login.canvas.height);
    var image = login.logo;
    var newWidth = image.width * login.scaleFactor;
    cxt.translate((image.width - newWidth) / 2.0, 0.0);
    cxt.scale(login.scaleFactor, 1.0);
    cxt.drawImage(image, 0.0, 0.0);
    cxt.restore();
}

//## updating ##################################################################################

function updateLoginAnimation() {
    if (data.state !== "login") {
        return;
    }
    var newUpdateTime = now();
    login.scaleFactor += login.scaleDirection * (newUpdateTime - login.lastUpdateTime) * 0.001 * 0.3;
    if (login.scaleFactor > 1.0) {
        login.scaleFactor = 1.0;
        login.scaleDirection = -1.0;
    }
    if (login.scaleFactor < -1.0) {
        login.scaleFactor = -1.0;
        login.scaleDirection = 1.0;
    }
    login.lastUpdateTime = newUpdateTime;
}

//## message handler helpers ###################################################################

function setLoginIncorrect() {
    login.username.style.backgroundColor = "#FF0000";
    login.password.style.backgroundColor = "#FF0000";
    login.username.focus();
}

function handleLoginJson(json) {
    if (json.Params.Success === "true") {
        data.playerName = login.username.value;
        login.username.style.backgroundColor = "#FFFFFF";
        login.password.style.backgroundColor = "#FFFFFF";
        game.entities = {};
        setStateToGame();
    } else {
        setLoginIncorrect();
    }
}

//## message handlers ##########################################################################

function onLoginButton() {
    if (login.username.value === "") {
        setLoginIncorrect();
        return;
    }
    wsSend({
               "Function": "login",
               "Params": {
                   "Username": login.username.value,
                   "Password": login.password.value
               }
           });
}

function onLoginKeyUp(keyunicode) {
    if (keyunicode == 13 /* enter */) {
        onLoginButton();
        return;
    }
}

//## page layout ###############################################################################

function positionLogin() {
    var width = 500;
    var height = 230;
    login.container.style.position = "fixed";
    login.container.style.width = toPx(width);
    login.container.style.height = toPx(height);
    login.container.style.top = toPx((window.innerHeight - height) / 2);
    login.container.style.left = toPx((window.innerWidth - width) / 2);
    login.controls.style.marginTop = toPx((height - login.controls.clientHeight) / 2);
}

//## initialization ############################################################################

function initializeLogin() {
    login.container = document.getElementById("logincontainer");
    login.canvas = document.getElementById("logincanvas");
    login.controls = document.getElementById("logincontrols");
    login.username = document.getElementById("loginusername");
    login.password = document.getElementById("loginpassword");
    login.button = document.getElementById("loginbutton");
    login.button.onclick = onLoginButton;
    login.logo = convertSvg("images/logo.svg");
    setInterval(updateLoginAnimation, 30);
    setInterval(drawLoginCanvas, 30);
}
