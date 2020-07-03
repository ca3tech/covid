initqstr = window.location.search;

$(document).on("shiny:sessioninitialized", function(event) {
  params = new URLSearchParams(initqstr);
  params.forEach(function(value, key) {
    Shiny.setInputValue(`q${key}`, value);
  })
})