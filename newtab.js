var sampleModel = JSON.parse('{"onboarded":true,"dob":"1996-04-02","death":3351283200000,"entries":[{"label":"full pizzas","outro":"left to eat","rate":7,"unit":"Month"},{"label":"phone calls home","outro":"left to make","rate":2.2,"unit":"Month"},{"label":"movies in theater","outro":"left to watch","rate":4,"unit":"Year"},{"label":"visits home","outro":"left to make","rate":2,"unit":"Year"},{"label":"sodas","outro":"left to drink","rate":1,"unit":"Week"},{"label":"emails","outro":"left to read","rate":20,"unit":"Day"},{"label":"sushi dinners","outro":"left to eat","rate":4,"unit":"Year"},{"label":"concerts","outro":"left to attend","rate":1,"unit":"Year"},{"label":"gym workouts","outro":"left","rate":2,"unit":"Week"},{"label":"glasses of water","outro":"left to drink","rate":3,"unit":"Day"},{"label":"beach trips","outro":"left to take","rate":1,"unit":"Year"},{"label":"hiking trips","outro":"left to take","rate":2,"unit":"Year"}],"activeEntry":{"entry":{"label":"sushi dinners","outro":"left to eat","rate":4,"unit":"Year"},"numleft":237.47783132686453}}')

var objectEquals = function(obj1, obj2) {
    return JSON.stringify(Object.keys(obj1).sort()) === JSON.stringify(Object.keys(obj2).sort());
}

var storedModel = localStorage.getItem('elm-stimulus-save');
var startingModel = storedModel ? JSON.parse(storedModel) : null;
if (startingModel !== null && !objectEquals(startingModel, sampleModel)) { // short circuit if model is already null
    startingModel = null;
}
var startingState = { rand: Math.floor( Math.random() * 100000 ), time: (new Date()).getTime(), savedModel: startingModel }

var stimulus = Elm.Stimulus.fullscreen(startingState);

if (stimulus !== null) {
    stimulus.ports.setStorage.subscribe(function(model) {
        localStorage.setItem('elm-stimulus-save', JSON.stringify(model));
    });

    chrome.runtime.onInstalled.addListener( function (details) {
        if (details.reason == "update" ) {
            localStorage.clear();
        }
    });
} else {
    document.getElementsByClassName("error body")[0].style.display = "block";
}
