document.addEventListener('DOMContentLoaded',function() {
  document.getElementById("table-of-contents").onclick = function() {
    var elem = document.getElementById("text-table-of-contents");
    elem.style.display = elem.style.display == "block" ? "none" : "block";
  }
});

