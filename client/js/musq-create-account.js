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
createaccount.faces.human.male.mouths = {};
createaccount.faces.buffers.push(createaccount.faces.human.male.mouths);
createaccount.faces.human.male.mouths.colors = [];
createaccount.faces.human.male.noses = {};
createaccount.faces.buffers.push(createaccount.faces.human.male.noses);
createaccount.faces.human.male.noses.colors = [];
createaccount.faces.human.scars = {};
createaccount.faces.human.scars.urls = [undefined];
createaccount.faces.buffers.push(createaccount.faces.human.scars);
createaccount.faces.human.scars.colors = ["#8c846a", "#362a1e"];
createaccount.faces.human.glasses = {};
createaccount.faces.human.glasses.urls = [undefined];
createaccount.faces.buffers.push(createaccount.faces.human.glasses);
createaccount.faces.human.glasses.colors = [];
createaccount.faces.human.male.hairs = {};
createaccount.faces.human.male.hairs.urls = [undefined];
createaccount.faces.buffers.push(createaccount.faces.human.male.hairs);
createaccount.faces.human.male.hairs.colors = ["#140d00"];
createaccount.faces.requestQueue = [];
createaccount.sliders = [];

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

function createAccountAddVisualControlSlider() {
    var sliderdiv = document.createElement("div");
    sliderdiv.setAttribute("class", "slider");
    sliderdiv.setAttribute("tabIndex", "1");
    createaccount.visualcontrols.appendChild(sliderdiv);
    var sliderinput = document.createElement("input");
    sliderinput.setAttribute("class", "slider-input");
    sliderdiv.appendChild(sliderinput);
    var slider = new Slider(sliderdiv, sliderinput);
    createaccount.sliders.push(slider);
}

function createAccountVisualRandomHelper(buffer) {
    var canvas = createaccount.canvas;
    if (buffer.urls) {
        buffer.url = randomElement(buffer.urls);
    } else {
        delete buffer.url;
    }
    if (buffer.colors) {
        buffer.color = randomElement(buffer.colors);
    } else {
        delete buffer.color;
    }
    if (buffer.url) {
        buffer.image = convertSvg(buffer.url, buffer.color, 2.0);
    }
}

function handleGetFilesJson(json) {
    if (createaccount.faces.requestQueue.length === 0) {
        log("Received unexpected getFiles.");
        return;
    }
    var buffer = createaccount.faces.requestQueue[0];
    if (!buffer.urls) {
        buffer.urls = [];
    }
    buffer.urls = buffer.urls.concat(json.Params.Images);
    createAccountVisualRandomHelper(buffer);
    createaccount.faces.requestQueue = createaccount.faces.requestQueue.slice(1);
    // [Randy 25/11/2010] TODO: Bind with buffer.urls.
    createAccountAddVisualControlSlider();
}

//## message handlers ##########################################################################

function onCreateAccountButton() {
    if (createaccount.password1.value !== createaccount.password2.value) {
        alert("Passwords don't match!");
        return;
    }
    var validpart = function (buffer) { return buffer.url; };
    wsSend({
               "Function": "createAccount",
               "Params": {
                   "Username": createaccount.username.value,
                   "Password": createaccount.password1.value,
                   "Email": createaccount.email.value,
                   "Images": createaccount.faces.buffers.filter(validpart).map(toVisualPart)
               }
           });
}

function onCreateAccountVisualRandom() {
    for (var bufferI in createaccount.faces.buffers) {
        var buffer = createaccount.faces.buffers[bufferI];
        createAccountVisualRandomHelper(buffer);
    }
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
    createaccount.visualcontrols = document.getElementById("createaccountvisualcontrols");
    document.getElementById("createaccountvisualrandom").onclick = onCreateAccountVisualRandom;
    setInterval(drawCreateAccountCanvas, 500);
}

function createAccountInitializeImages() {
    requestImageUrls("images/faces/human/male/", "face*.svg", createaccount.faces.human.male.faces);
    requestImageUrls("images/faces/human/male/", "ears*.svg", createaccount.faces.human.male.ears);
    requestImageUrls("images/faces/human/male/", "eyes*.svg", createaccount.faces.human.male.eyes);
    requestImageUrls("images/faces/human/male/", "hair*.svg", createaccount.faces.human.male.hairs);
    requestImageUrls("images/faces/human/male/", "mouth*.svg", createaccount.faces.human.male.mouths);
    requestImageUrls("images/faces/human/male/", "nose*.svg", createaccount.faces.human.male.noses);
    requestImageUrls("images/faces/human/", "scar*.svg", createaccount.faces.human.scars);
    requestImageUrls("images/faces/human/", "glasses*.svg", createaccount.faces.human.glasses);
}
