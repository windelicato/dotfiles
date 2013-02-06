var installPageVersion = 2;
BGcall("storage_set", "saw_install_page", installPageVersion);

$("#cleaner-warning a").click(function() {
  alert(translate("filecleanerwarning"));
});

$(".nav a").click(function() {
  showCard(this.name);
  return false;
});

// Fade out any visible cards and fade in the card with ID |cardId|.
function showCard(cardId) {
  var card = $("#" + cardId);
  function show() {
    card.fadeIn();
  }
  var visible = $("#.card:visible");
  if (visible.length)
    visible.fadeOut(show);
  else
    show();
}

// Show a loading progress indicator for a few seconds while the user
// gets her bearings.
function showLoadingBar() {
  var start = Date.now();
  (function() {

    var runLength = 2500; // Should take this many ms
    var pctTime = (Date.now() - start) / runLength; // Goes from 0 to 1

    // Start slow, then speed up.
    var pctDone = Math.pow(pctTime, 4);

    var bar = $("#chrome-loading-progress");
    bar[0].value = pctDone;
    if (SAFARI) { // progress bar not yet supported
      $("#chrome-loading-progress").css({"background-color": "#ccc", "border": "1px solid black"});
      $("#safari-loading-progress").css("width", Math.min(100, Math.round(pctDone * 100)) + "%");
    }

    if (pctDone < 1) {
      window.setTimeout(arguments.callee, 20);
      return;
    }

    window.setTimeout(function() {
      // Loaded
      $("#loading-wrapper").
        find("#done").fadeIn().end().
        delay(1800).
        fadeOut(function() {
          $("#header").fadeIn();
          showCard("last-step");
        });
    }, 200);
  })();
}

window.onmessage = function(e) {
  if (e.origin !== "https://chromeadblock.com")
    return;
  if (e.data.command !== "delay")
    return;
  var when = Date.now() + e.data.minutes * 60E3;
  BGcall("show_delayed_payment_request_at", when);
  showCard("later");
  if (SAFARI)
    $("li.video").hide();
};

var delayed = /[&?]delayed/.test(document.location.search);

// Load iframe
var userId = (document.location.search.match(/\u\=(\w+)/) || [])[1];
var iframe = $("<iframe>").
  attr("src", "https://chromeadblock.com/pay/?source=I&header=install&u=" + 
       userId + (delayed ? "&delayed" : "") + "&x=33").
  attr("frameBorder", 0).
  attr("scrolling", "no").
  width(750).
  height(450);
$("#iframe-slot").html(iframe);

if (delayed) {
  $("#loading-wrapper").hide();
  $("#header-delayed").fadeIn();
  var cardId = "loading-delayed";
  showCard(cardId);
  $("#" + cardId).delay(3500).promise().then(function() {
    showCard("last-step");
  });
}
else {
  showLoadingBar();
}

localizePage();
