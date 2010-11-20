//## global data ###############################################################################

createaccount = {};
createaccount.faces = {};
createaccount.faces.buffers = [];
createaccount.faces.human = {};
createaccount.faces.human.male = {};
createaccount.faces.human.male.ears = {};
createaccount.faces.buffers.push(createaccount.faces.human.male.ears);
createaccount.faces.human.male.ears.colors = ["#fff0c1", "#785d42"];
createaccount.faces.human.male.faces = {};
createaccount.faces.buffers.push(createaccount.faces.human.male.faces);
createaccount.faces.human.male.faces.colors = ["#fff0c1", "#785d42"];
createaccount.faces.human.male.eyes = {};
createaccount.faces.buffers.push(createaccount.faces.human.male.eyes);
createaccount.faces.human.male.eyes.colors = ["#000000", "#000044"];
createaccount.faces.human.male.hairs = {};
createaccount.faces.buffers.push(createaccount.faces.human.male.hairs);
createaccount.faces.human.male.hairs.colors = ["#140d00"];
createaccount.faces.human.male.mouths = {};
createaccount.faces.buffers.push(createaccount.faces.human.male.mouths);
createaccount.faces.human.male.mouths.colors = [];
createaccount.faces.human.male.noses = {};
createaccount.faces.buffers.push(createaccount.faces.human.male.noses);
createaccount.faces.human.male.noses.colors = [];
createaccount.faces.requestQueue = [];

//## utilities that depend on the global data ##################################################

function requestImageUrls(baseurl, wildcard, targetarray) {
    createaccount.faces.requestQueue.push(targetarray);
    wsSend({
               "Function": "getFiles",
               "Params": {
                   "BasePath": baseurl,
                   "WildCard": wildcard
               }
           });
}

function toVisualPart(buffer) {
    if (buffer.color) {
        return { "Url": buffer.url, "Color": buffer.color };
    }
    return { "Url": buffer.url };
}

//## drawing ###################################################################################

function drawCreateAccountCanvas() {
    if (data.state !== "createaccount") {
        return;
    }
    var canvas = createaccount.canvas;
    var cxt = canvas.getContext("2d");
    cxt.save();
    cxt.clearRect(0, 0, canvas.width, canvas.height);
    cxt.fillStyle = "#ff0000";
    for (var bufferI in createaccount.faces.buffers) {
        var buffer = createaccount.faces.buffers[bufferI];
        drawIfExists(cxt, buffer.image, 0, 0);
    }
    cxt.restore();
}

//## message handler helpers ###################################################################

function handleCreateAccountJson(json) {
    if (json.Params.Success === "true") {
        setStateToLogin();
    } else {
        alert("Account creation failed: " + json.Params.Reason);
    }
}

function handleGetFilesJson(json) {
    if (createaccount.faces.requestQueue.length === 0) {
        log("Received unexpected getFiles.");
        return;
    }
    var buffer = createaccount.faces.requestQueue[0];
    buffer.urls = json.Params.Images;
    buffer.url = randomElement(buffer.urls);
    buffer.color = randomElement(buffer.colors);
    buffer.image = convertSvg(buffer.url, buffer.color);
    createaccount.faces.requestQueue = createaccount.faces.requestQueue.slice(1);
}

//## message handlers ##########################################################################

function onCreateAccountButton() {
    if (createaccount.password1.value !== createaccount.password2.value) {
        alert("Passwords don't match!");
        return;
    }
    wsSend({
               "Function": "createAccount",
               "Params":
               {
                   "Username": createaccount.username.value,
                   "Password": createaccount.password1.value,
                   "Email": createaccount.email.value,
                   "Images": createaccount.faces.buffers.map(toVisualPart)
               }
           });
}

//## initialization ############################################################################

function initializeCreateAccount() {
    createaccount.container = document.getElementById("createaccountcontainer");
    createaccount.username = document.getElementById("createaccountusername");
    createaccount.button = document.getElementById("createaccountbutton");
    createaccount.button.onclick = onCreateAccountButton;
    createaccount.password1 = document.getElementById("createaccountpassword1");
    createaccount.password2 = document.getElementById("createaccountpassword2");
    createaccount.email = document.getElementById("createaccountemail");
    createaccount.canvas = document.getElementById("createaccountcanvas");
    setInterval(drawCreateAccountCanvas, 500);
}
