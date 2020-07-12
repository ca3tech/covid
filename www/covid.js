initqstr = window.location.search;

$(document).on("shiny:sessioninitialized", function(event) {
  var params = new URLSearchParams(initqstr);
  params.forEach(function(value, key) {
    Shiny.setInputValue(`q${key}`, value);
  });
});

function toggleVisibility(id) {
  var el = $(`#${id}`);
  if(el != null) {
    el.toggle();
  }
}
Shiny.addCustomMessageHandler("toggleVisibility", toggleVisibility);
