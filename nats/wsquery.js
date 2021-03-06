var nats = NATS.connection({},{});

function emptyDiv(div) {
    while (div.childNodes.length > 0) {
        div.removeChild(div.childNodes[0]);
    }
}

var topic = '';
var block = 0;
var total = -1;
var wkid = '';
var bytes = 0;
var assembled = [];

var prefix = Math.floor(Math.random() * 999999).toString();
var assembled = [];

function render(file) {
    console.log('render to html');
    var stripped = file;
    do {
        file = stripped;
        stripped = file.replace(stripRE, '');
    } while (stripped != file);
    var articleDiv = document.getElementById('article');
    emptyDiv(articleDiv);
    articleDiv.innerHTML = wiky.process(stripped);
}

nats.subscribe('wikiresult', function(mtext) {
    console.log(mtext);
    var msg = JSON.parse(mtext);
    if (msg.wkid === wkid) {
        block = block + 1;
        var t = decodeURIComponent(msg.text);
        bytes += t.length;
        total = msg.total;
        assembled.push(t);
        if (msg.text != '') {
            var articleDiv = document.getElementById('article');
            emptyDiv(articleDiv);
            articleDiv.appendChild(document.createTextNode('Loading... '+bytes+' out of '+total));
            requestBlock();
        } else {
            var text = assembled.join("");
            render(text);
        }
    }
});

function requestBlock() {
    wkid = prefix + '_' + block;
    var request = {
        wkid: wkid,
        topic: topic,
        block: block
    };
    var requestText = JSON.stringify(request);
    nats.publish('wikipedia',requestText);
}

function query(topic_) {
    topic = topic_;
    block = 0;
    total = -1;
    bytes = 0;
    assembled = [];
    requestBlock();
}

function runSearch(topic_) {
    var articleDiv = document.getElementById('article');
    emptyDiv(articleDiv);
    articleDiv.appendChild(document.createTextNode('Loading...'));
    query(topic_);
}

var sp = document.getElementById('search-perform');
sp.addEventListener('click', function() {
    var searchDiv = document.getElementById('search-entry');
    query(searchDiv.value);
    searchDiv.blur();
});
var se = document.getElementById('search-entry');
se.addEventListener('keydown', function(evt) {
    if (evt.keyCode == 13) {
        var searchDiv = document.getElementById('search-entry');
        query(searchDiv.value);
        searchDiv.blur();
    }
});
