//## global data ###############################################################################

createaccount = {};
createaccount.faces = {};
createaccount.faces.buffers = [];
createaccount.faces.human = {};
createaccount.faces.human.male = {};

createaccount.faces.human.male.ears = {};
createaccount.faces.human.male.ears.baseurl = "images/faces/human/male/";
createaccount.faces.human.male.ears.wildcard = "ears*.svg";
createaccount.faces.human.male.ears.colors = {};
createaccount.faces.human.male.ears.colors.choices = ["#fff0c1", "#fbc38d", "#a76f38", "#785d42", "#f8f7a1"];
createaccount.faces.buffers.push(createaccount.faces.human.male.ears);

createaccount.faces.human.male.faces = {};
createaccount.faces.human.male.faces.baseurl = "images/faces/human/male/";
createaccount.faces.human.male.faces.wildcard = "face*.svg";
createaccount.faces.human.male.faces.colors = {};
createaccount.faces.human.male.faces.colors.choices = createaccount.faces.human.male.ears.colors.choices;
createaccount.faces.buffers.push(createaccount.faces.human.male.faces);

createaccount.faces.human.male.eyes = {};
createaccount.faces.human.male.eyes.baseurl = "images/faces/human/male/";
createaccount.faces.human.male.eyes.wildcard = "eyes*.svg";
createaccount.faces.human.male.eyes.colors = {};
createaccount.faces.human.male.eyes.colors.choices = ["#000000", "#000044", "#005dac", "#116600"];
createaccount.faces.buffers.push(createaccount.faces.human.male.eyes);

createaccount.faces.human.male.mouths = {};
createaccount.faces.human.male.mouths.baseurl = "images/faces/human/male/";
createaccount.faces.human.male.mouths.wildcard = "mouth*.svg";
createaccount.faces.buffers.push(createaccount.faces.human.male.mouths);

createaccount.faces.human.male.noses = {};
createaccount.faces.human.male.noses.baseurl = "images/faces/human/male/";
createaccount.faces.human.male.noses.wildcard = "nose*.svg";
createaccount.faces.buffers.push(createaccount.faces.human.male.noses);

createaccount.faces.human.scars = {};
createaccount.faces.human.scars.baseurl = "images/faces/human/";
createaccount.faces.human.scars.wildcard = "scar*.svg";
createaccount.faces.human.scars.urls = {};
createaccount.faces.human.scars.urls.choices = [undefined];
createaccount.faces.human.scars.colors = {};
createaccount.faces.human.scars.colors.choices = ["#8c846a", "#362a1e"];
createaccount.faces.buffers.push(createaccount.faces.human.scars);

createaccount.faces.human.glasses = {};
createaccount.faces.human.glasses.baseurl = "images/faces/human/";
createaccount.faces.human.glasses.wildcard = "glasses*.svg";
createaccount.faces.human.glasses.urls = {};
createaccount.faces.human.glasses.urls.choices = [undefined];
createaccount.faces.buffers.push(createaccount.faces.human.glasses);

createaccount.faces.human.male.hairs = {};
createaccount.faces.human.male.hairs.baseurl = "images/faces/human/male/";
createaccount.faces.human.male.hairs.wildcard = "hair*.svg";
createaccount.faces.human.male.hairs.urls = {};
createaccount.faces.human.male.hairs.urls.choices = [undefined];
createaccount.faces.human.male.hairs.colors = {};
createaccount.faces.human.male.hairs.colors.choices = ["#140d00", "#5c2110", "#ed2713", "#ede249"];
createaccount.faces.buffers.push(createaccount.faces.human.male.hairs);

createaccount.faces.human.male.beards = {};
createaccount.faces.human.male.beards.baseurl = "images/faces/human/male/";
createaccount.faces.human.male.beards.wildcard = "beard*.svg";
createaccount.faces.human.male.beards.urls = {};
createaccount.faces.human.male.beards.urls.choices = [undefined];
createaccount.faces.human.male.beards.colors = {};
createaccount.faces.human.male.beards.colors.choices = createaccount.faces.human.male.hairs.colors;
createaccount.faces.buffers.push(createaccount.faces.human.male.beards);

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

function createAccountUpdateBufferImage(buffer) {
    if (buffer.urls && buffer.urls.choice && buffer.urls.choice.value) {
        if (buffer.colors && buffer.colors.choice) {
            buffer.image = convertSvg(buffer.urls.choice.value, buffer.colors.choice.value, 2.0);
        } else {
            buffer.image = convertSvg(buffer.urls.choice.value, undefined, 2.0);
        }
    } else {
        delete buffer.image;
    }
}

function createAccountSliderValueChanged(slider, selection, buffer) {
    selection.choice = {};
    selection.choice.index = slider.getValue();
    selection.choice.value = selection.choices[selection.choice.index];
    createAccountUpdateBufferImage(buffer);
}

function createAccountAddVisualControlSlider(selection, buffer) {
    if (selection.choices.length < 2) {
        return undefined;
    }
    var sliderdiv = document.createElement("div");
    sliderdiv.setAttribute("class", "slider");
    sliderdiv.setAttribute("tabIndex", "1");
    createaccount.visualcontrols.appendChild(sliderdiv);
    var sliderinput = document.createElement("input");
    sliderinput.setAttribute("class", "slider-input");
    sliderdiv.appendChild(sliderinput);
    var slider = new Slider(sliderdiv, sliderinput);
    createaccount.sliders.push(slider);
    slider.setMinimum(0);
    slider.setMaximum(selection.choices.length - 1);
    slider.onchange = function () { createAccountSliderValueChanged(slider, selection, buffer); };
    return slider;
}

function createAccountSliderDataToUi(slider, selection) {
    if (!slider || !selection || !selection.choice) {
        return;
    }
    slider.setValue(selection.choice.index);
}

function createAccountVisualRandomHelper(buffer) {
    if (buffer.urls) {
        if (buffer.urls.choices) {
            buffer.urls.choice = randomElement(buffer.urls.choices);
        } else {
            delete buffer.urls.choice;
        }
    }
    if (buffer.colors) {
        if (buffer.colors.choices) {
            buffer.colors.choice = randomElement(buffer.colors.choices);
        } else {
            delete buffer.colors.choice;
        }
    }
    createAccountUpdateBufferImage(buffer);
}

function handleGetFilesJson(json) {
    if (createaccount.faces.requestQueue.length === 0) {
        log("Received unexpected getFiles.");
        return;
    }
    var buffer = createaccount.faces.requestQueue[0];
    if (!buffer.urls) {
        buffer.urls = {};
        buffer.urls.choices = [];
    }
    buffer.urls.choices = buffer.urls.choices.concat(json.Params.Images);
    createAccountVisualRandomHelper(buffer);
    createaccount.faces.requestQueue = createaccount.faces.requestQueue.slice(1);
    buffer.urlslider = createAccountAddVisualControlSlider(buffer.urls, buffer);
    createAccountSliderDataToUi(buffer.urlslider, buffer.urls);
    if (buffer.colors) {
        buffer.colorslider = createAccountAddVisualControlSlider(buffer.colors, buffer);
        createAccountSliderDataToUi(buffer.colorslider, buffer.colors);
    }
}

//## message handlers ##########################################################################

function onCreateAccountButton() {
    if (createaccount.password1.value !== createaccount.password2.value) {
        alert("Passwords don't match!");
        return;
    }
    var validpart = function (buffer) {
        return buffer.urls && buffer.urls.choice && buffer.urls.choice.value;
    };
    var toVisualPart = function (buffer) {
        if (buffer.colors && buffer.colors.choice && buffer.colors.choice.value) {
            return { "Url": buffer.urls.choice.value, "Color": buffer.colors.choice.value };
        }
        return { "Url": buffer.urls.choice.value };
    };
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
        createAccountSliderDataToUi(buffer.urlslider, buffer.urls);
        createAccountSliderDataToUi(buffer.colorslider, buffer.colors);
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
    for (var bufferI in createaccount.faces.buffers) {
        var buffer = createaccount.faces.buffers[bufferI];
        requestImageUrls(buffer.baseurl, buffer.wildcard, buffer);
    }
}
