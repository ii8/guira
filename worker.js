importScripts('guira.js')

onmessage = function(e) {
  var start = new Date();
  var current = new Date();
  var i = 100;

  while (i) {
    var b = false;

    try {
      b = guira(start.toISOString(), "day", e.data, current.toISOString());
    } catch (ex) {
      postMessage("error");
      break;
    }

    if (b)
      postMessage(i-- == 1 ? "..." : current.toDateString());

    current.setDate(current.getDate() + 1);
  }
}
