function promiseOneObject(url,api,headers) {
    var curTime = (new Date()).getTime();
    if (api && lastRequest + requestInterval > curTime) {
        return Q.delay(lastRequest + requestInterval - curTime).then(function() {
            return promiseOneObject(url,api,headers);
        });
    }
    lastRequest = curTime;
    var d = Q.defer();
    var request = new XMLHttpRequest();
    request.addEventListener('error', function() {
        console.error(request.status,request.responseText);
        d.reject({status: request.status, text: "could not load " + name});
    });
    request.addEventListener('load', function() {
        if (request.status != 200) {
            console.error("Error", request.status,request.responseText, url);
            d.reject({status: request.status, text: request.responseText});
        } else {
            console.log("Success", url);
            d.resolve(request.responseText);
        }
    });
    request.open('GET', url);
    if (headers) {
        for (var k in headers) {
            request.setRequestHeader(k, headers[k]);
        }
    }
    request.send();
    return d.promise;
}

var stripRE = /{[ \t\r\n]*[^{}]*[ \t\r\n]*}/gm ;

function getAndRenderLocalFile(f) {
    promiseOneObject(f).then(function(file) {
        render(file);
    });
}
