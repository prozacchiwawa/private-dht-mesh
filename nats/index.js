// NATS example
var nats = NATS.connection({ verbose: true }, {});

// Simple Publisher
nats.publish('foo', 'Hello World!');

// Simple Subscriber
nats.subscribe('foo', function(msg) {
    var odiv = document.getElementById('chatarea');
    var newDiv = document.createElement('div');
    var tdiv = document.createTextNode('Received a message: ' + msg);
    newDiv.appendChild(tdiv);
    odiv.appendChild(newDiv);  
});

function sendInput() {
    var inp = document.getElementById('typehere');
    var val = inp.value;
    inp.value = "";
    nats.publish('foo', val);
}

document.getElementById('send').addEventListener('click', function(evt) {
    sendInput();
});

document.getElementById('typehere').addEventListener('keydown', function(evt) {
    if (evt.keyCode == 13) {
        sendInput();
    }
});
