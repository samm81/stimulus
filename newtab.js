var storedModel = localStorage.getItem('elm-stimulus-save');
var startingModel = storedModel ? JSON.parse(storedModel) : null;
var startingState = { rand: Math.floor( Math.random() * 100000 ), savedModel: startingModel }
var stimulus = Elm.Stimulus.fullscreen(startingState);
stimulus.ports.setStorage.subscribe(function(model) {
    localStorage.setItem('elm-stimulus-save', JSON.stringify(model));
});
