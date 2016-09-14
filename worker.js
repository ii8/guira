importScripts('guira.js')

onmessage = function(e) {
  var start = new Date();
  var current = new Date();
  var i = 100;
  var first = true;

  while (i && current.getFullYear() < 10000) {
    var b = false;

    try {
      b = guira(start.toISOString(), "day", e.data, current.toISOString());
    } catch (ex) {
      postMessage("error");
      break;
    }

    if (first) {
      postMessage("success");
      first = false;
    }

    if (b)
      postMessage(i-- == 1 ? "..." : current.toDateString());

    current.setDate(current.getDate() + 1);
  }
}
