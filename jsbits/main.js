//#OPTIONS: CPP

var runGameStep_

const canvas = document.getElementById('GameCanvas');
const context = canvas.getContext('2d');

var pressed = {};
var mouseX = 0;
var mouseY = 0;
var mouseDown = false;

document.addEventListener('keydown', (event) => {
    var name = event.key;
    var code = event.code;
    pressed[code] = true;
  }, false);

document.addEventListener('keyup', (event) => {
    var name = event.key;
    var code = event.code;
    delete pressed[code];
  }, false);

// Add event listener on mouse position
document.addEventListener('mousemove', (event) => {
    var rect = canvas.getBoundingClientRect();
    mouseX = event.clientX - rect.left;
    mouseY = event.clientY - rect.top;
})

document.addEventListener('mousedown', (event) => {
    mouseDown = true;
})
document.addEventListener('mouseup', (event) => {
    mouseDown = false;
})

function setRunGameStep(rgs) {
  runGameStep_ = rgs;
}

function runGameStep(x, y, pressed, deltaTime) {
  var args = {
    x: x,
    y: y,
    pressed: pressed,
    deltaTime: deltaTime
  }
  return runGameStep_(args);
}

function unpackGameStepArgs(args) {
  RETURN_UBX_TUP4(args.x, args.y, args.pressed, args.deltaTime);
}

function h$arc(x, y, radius, startAngle, endAngle, counterclockwise) {
    context.arc(x, y, radius, startAngle, endAngle, counterclockwise);
}

function h$ellipse(x, y, radiusX, radiusY, rotation, startAngle, endAngle, counterclockwise) {
    context.ellipse(x, y, radiusX, radiusY, rotation, startAngle, endAngle, counterclockwise);
}

function h$fill() {
    context.fill();
}

function h$beginPath() {
    context.beginPath();
}

function h$closePath() {
    context.closePath();
}

function h$stroke() {
    context.stroke();
}

function h$moveTo(x, y) {
    context.moveTo(x, y);
}

function h$lineTo(x, y) {
    context.lineTo(x, y);
}

function h$clearCanvas(colR, colG, colB) {
    context.fillStyle=`rgb(${colR},${colG},${colB})`;
    context.fillRect(0, 0, canvas.width, canvas.height);
}

function h$fillStyle(colR, colG, colB) {
    context.fillStyle=`rgb(${colR},${colG},${colB})`;
}

function h$strokeStyle(colR, colG, colB) {
    context.strokeStyle=`rgb(${colR},${colG},${colB})`;
}

function h$fillRect(x, y, width, height) {
    context.fillRect(x, y, width, height);
}

function h$getCanvasWidth() {
    return canvas.width;
}

function h$getCanvasHeight() {
    return canvas.height;
}

function setFont(text) {
    context.font = text;
}

function fillText(text, x, y, maxWidth) {
    context.fillText(text, x, y, maxWidth);
}

function h$setLineWidth(lineWidth) {
    context.lineWidth = lineWidth;
}

function runGame() {
    var previousTimeStamp = null;

    function step(timeStamp) {
        if (!previousTimeStamp) {
            previousTimeStamp = timeStamp;
        }
        const deltaTime = (timeStamp-previousTimeStamp)/1000;
        runGameStep(mouseX, mouseY, mouseDown, deltaTime);
        previousTimeStamp = timeStamp;
        window.requestAnimationFrame(step);
    }
    window.requestAnimationFrame(step);
}