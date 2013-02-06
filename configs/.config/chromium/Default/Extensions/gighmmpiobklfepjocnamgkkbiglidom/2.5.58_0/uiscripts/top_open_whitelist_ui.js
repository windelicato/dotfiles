// Global lock so we can't open more than once on a tab.
if (typeof may_open_dialog_ui === "undefined")
    may_open_dialog_ui = true;

function top_open_whitelist_ui() {
  if (!may_open_dialog_ui)
    return;

  may_open_dialog_ui = false;

  var domain = document.location.host;

  // Safari's document.location breaks in the feed reader if the feed is fetched via https. Normal
  // feeds have URLs with scheme replaced with "feed", but HTTPS feeds have the scheme replaced with
  // "feed:https", which isn't exactly a valid scheme. Safari's document.location can't handle that,
  // so its domain property will be empty; fortunately, it puts a proper https url into the pathname
  // property and we can use it.
  if (SAFARI && domain === "" && document.location.href.indexOf("feed:https") === 0) {
    domain = parseUri(document.location.pathname).host;
  }

  // Get Flash objects out of the way of our UI
  BGcall('emit_page_broadcast', {fn:'send_content_to_back', options:{}});
  
  // defined in blacklister.js
  load_jquery_ui(function() {
    var btns = {};
    btns[translate("buttoncancel")] = function() { page.dialog('close');}
    btns[translate("buttonexclude")] = 
        function() {
          var filter = '@@||' + generateUrl() + '$document';
          BGcall('add_custom_filter', filter, function() {
            document.location.reload();
          });
        }

    var page = $("<div>").
      append('<span>' + translate("adblock_wont_run_on_pages_matching") + 
             '</span>').
      append('<br/><br/><i id="domainpart"></i><i id="pathpart"></i>').
      append("<br/><br/><br/><span id='whitelister_dirs'>" + 
             translate('you_can_slide_to_change') + "</span>").
      append('<br/><span id="modifydomain">' + translate('modifydomain') +
             "<input id='domainslider' type='range' min='0' value='0'/></span>").
      append('<span id="modifypath">' + translate('modifypath') +
             "<input id='pathslider' type='range' min='0' value='0'/></span>").
      dialog({
        title: translate("whitelistertitle2"),
        width: 600,
        minHeight: 130,
        buttons: btns,
        close: function() {
          may_open_dialog_ui = true;
          $(".adblock-ui-stylesheet").remove();
          page.remove();
        }
      });

    var fixedDomainPart = parseUri.secondLevelDomainOnly(domain, true);
    var domainparts = domain.substr(0, domain.lastIndexOf(fixedDomainPart)).split('.');
    domainparts.splice(domainparts.length-1, 1, fixedDomainPart);

    var location = document.location.pathname.match(/(.*?)(\/?)(\?|$)/);
    var pathparts = location[1].split('/');

    // Don't show the domain slider on
    // - sites without a third level domain name (e.g. foo.com)
    // - sites with an ip domain (e.g. 1.2.3.4)
    // Don't show the location slider on domain-only locations
    var noThirdLevelDomain = (domainparts.length === 1);
    var domainIsIp = /^(\d+\.){3}\d+$/.test(domain);
    var showDomain = !(noThirdLevelDomain || domainIsIp);
    $("#modifydomain", page).toggle(showDomain);
    var showPath = !!(location[1]);
    $("#modifypath", page).toggle(showPath);
    $("#whitelister_dirs", page).toggle(showDomain || showPath);

    $("#domainslider", page).
      attr("max", Math.max(domainparts.length - 1, 1));
    $("#pathslider", page).
      attr("max", Math.max(pathparts.length - 1, 1));
    $("#pathslider, #domainslider", page).
      change(onSliderChange);

    function onSliderChange() {
      generateUrl(true);
    }
    onSliderChange();

    // Generate the URL. If forDisplay is true, then it will truncate long URLs
    function generateUrl(forDisplay) {
      var result = "";
      var domainsliderValue = $("#domainslider", page)[0].valueAsNumber;
      var pathsliderValue = $("#pathslider", page)[0].valueAsNumber;

      // Make clear that it includes subdomains
      if (forDisplay && domainsliderValue != 0)
        result = "*.";

      // Append the chosen parts of a domain
      for (var i = domainsliderValue; i<=(domainparts.length - 2); i++) 
        result += domainparts[i] + '.';
      result += domainparts[domainparts.length - 1];
      for (var i = 1; i<=pathsliderValue; i++) 
        result += '/' + pathparts[i];

      // Append a final slash for for example filehippo.com/download_dropbox/
      if (pathparts.length != pathsliderValue + 1 || !location[1]) {
        result += "/";
        if (forDisplay)
          result += "*";
      } else {
        if (location[2])
          result += location[2];
      }

      if (forDisplay) {
        result = result.replace(/(\/[^\/]{6})[^\/]{3,}([^\/]{6})/g, '$1...$2');
        if (result.indexOf("/") > 30 && result.length >=60)
          result = result.replace(/^([^\/]{20})[^\/]+([^\/]{6}\/)/, '$1...$2')
        while (result.length >= 60)
          result = result.replace(/(\/.{4}).*?\/.*?(.{4})(?:\/|$)/, '$1...$2/');
        var domainpart = result.match(/^[^\/]+/)[0];
        var pathpart = result.match(/\/.*$/)[0];
        $("#domainpart", page).text(domainpart);
        $("#pathpart", page).text(pathpart);
      } else
        return result;
    }
  });
}

//@ sourceURL=/uiscripts/top_open_whitelist_ui.js
