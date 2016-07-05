/**
 * Created by Jon on 22/06/16.
 */



var node = document.getElementById("d3basic");
var app = Elm.TestInterop.embed(node);

app.ports.check.subscribe( data => {

  var dataSz = [];

  dataSz[0] = data[0].map(val => {
    return {name: val.name, value: 9, sz: 8}
  });

  dataSz[1] = data[1].map(val => {
    return {name: val.name, value: 9, sz: 6}
  });

  dataSz[2] = data[2].map(val => {
    return {name: val.name, value: 9, sz: 4}
  });

  showCircle(dataSz);

  var names = data[0].map(val => val.name);

  // var dataProcessedItems = [names[0].toUpperCase()];
  var dataProcessedItems = ["2"];
  app.ports.dataProcessedItems.send(dataProcessedItems);
});
