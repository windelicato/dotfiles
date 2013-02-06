function load_options() {
  // Check or uncheck each option.
  BGcall("get_settings", function(settings) {
    optionalSettings = settings;
    $("#tabpages").
      tabs({ 
        spinner: "",
        cache: true,
        cookie: {},
        load: function(event, ui) {
          //translation
          localizePage();

          // Toggle won't handle .advanced.chrome-only
          if (!optionalSettings.show_advanced_options)
            $(".advanced").hide();
          if (SAFARI)
            $(".chrome-only").hide();

          // Must load tab .js here: CSP won't let injected html inject <script>
          // see index.html:data-scripts
          ui.tab.dataset.scripts.split(' ').forEach(function(scriptToLoad) {
            // CSP blocks eval, which $().append(scriptTag) uses
            var s = document.createElement("script");
            s.src = scriptToLoad;
            document.body.appendChild(s);
          });
        },
      }).
      show();
  });
}

function displayVersionNumber() {
  try {
    var xhr = new XMLHttpRequest();
    xhr.open("GET", chrome.extension.getURL('manifest.json'), true);
    xhr.onreadystatechange = function() {
      if(this.readyState == 4) {
        var theManifest = JSON.parse(this.responseText);
        $("#version_number").text(translate("optionsversion", [theManifest.version]));
      }
    };
    xhr.send();
  } catch (ex) {} // silently fail
}

$('#paymentlink').click(function() {
  BGcall("storage_get", "userid", function(userId) {
    var href = "https://chromeadblock.com/pay/?source=O&u=" + userId;
    BGcall("openTab", href);
  });
  return false;
});

if (navigator.language.substring(0, 2) != "en")
  $("#translation_credits").text(translate("translator_credit"));

if (SAFARI && LEGACY_SAFARI) {
  if (navigator.appVersion.indexOf("Mac OS X 10_5_") !== -1) {
    // Safari 5.1 isn't available on Leopard (OS X 10.5). Don't urge the users to upgrade in this case.
  } else {
    $("#safari50_updatenotice").show();
  }
}

var optionalSettings = {};
load_options();
displayVersionNumber();
localizePage();
