var elementPurger = {
  onPurgeRequest: function(request, sender, sendResponse) {
    if (request.command === 'purge-elements' &&
        request.frameUrl === document.location.href.replace(/#.*$/, ""))
      elementPurger._purgeElements(request);

    sendResponse({});
  },

  // Remove elements on the page of |request.elType| that request
  // |request.url|.  Will try again if none are found unless |lastTry|.
  _purgeElements: function(request, lastTry) {
    var elType = request.elType;
    var url = request.url;

    log("[DEBUG]", "Purging:", lastTry, elType, url);

    var tags = {};
    tags[ElementTypes.image] = { IMG:1 };
    tags[ElementTypes.subdocument] = { IFRAME:1, FRAME: 1 };
    tags[ElementTypes.object] = { "OBJECT":1, EMBED:1 };

    var srcdata = this._srcsFor(url);
    for (var i=0; i < srcdata.length; i++) {
      for (var tag in tags[elType]) {
        var src = srcdata[i];
        var attr = (tag === "OBJECT" ? "data" : "src");
        var selector = tag + '[' + attr + src.op + '"' + src.text + '"]';

        var results = document.querySelectorAll(selector);
        log("[DEBUG]", "  ", results.length, "results for selector:", selector);
        if (results.length) {
          for (var j=0; j < results.length; j++) {
            destroyElement(results[j], elType);
          }
          var externalId = "kodkhcagmjcidjgljmbfiaconnbnohho";
          request.selector = selector;
          chrome.extension.sendRequest(externalId, request);
          
          return; // I doubt the same URL was loaded via 2 different src attrs.
        }
      }
    }

    // No match; try later.  We may still miss it (race condition) in which
    // case we give up, rather than polling every second or waiting 10 secs
    // and causing a jarring page re-layout.
    if (!lastTry) {
      var that = this;
      setTimeout(function() { that._purgeElements(request, true); }, 2000);
    }
  },

  // Return a list of { op, text }, where op is a CSS selector operator and
  // text is the text to select in a src attr, in order to match an element on
  // this page that could request the given absolute |url|.
  _srcsFor: function(url) {
    // NB: <img src="a#b"> causes a request for "a", not "a#b".  I'm
    // intentionally ignoring IMG tags that uselessly specify a fragment.
    // AdBlock will fail to hide them after blocking the image.
    var url_parts = parseUri(url), page_parts = this._page_location;
    var results = [];
    // Case 1: absolute (of the form "abc://de.f/ghi" or "//de.f/ghi")
    results.push({ op:"$=", text: url.match(/\:(\/\/.*)$/)[1] });
    if (url_parts.hostname === page_parts.hostname) {
      var url_search_and_hash = url_parts.search + url_parts.hash;
      // Case 2: The kind that starts with '/'
      results.push({ op:"=", text: url_parts.pathname + url_search_and_hash });
      // Case 3: Relative URL (of the form "ab.cd", "./ab.cd", "../ab.cd" and
      // "./../ab.cd")
      var page_dirs = page_parts.pathname.split('/');
      var url_dirs = url_parts.pathname.split('/');
      var i = 0;
      while (page_dirs[i] === url_dirs[i] 
             && i < page_dirs.length - 1 
             && i < url_dirs.length - 1) {
        i++; // i is set to first differing position
      }
      var dir = new Array(page_dirs.length - i).join("../");
      var path = url_dirs.slice(i).join("/") + url_search_and_hash;
      if (dir) {
        results.push({ op:"$=", text: dir + path });
      } else {
        results.push({ op:"=", text: path });
        results.push({ op:"=", text: "./" + path });
      }
    }

    return results;
  },

  // To enable testing
  _page_location: document.location
};

adblock_begin({
  startPurger: function() {
    chrome.extension.onRequest.addListener(elementPurger.onPurgeRequest);
  },
  stopPurger: function() {
    chrome.extension.onRequest.removeListener(elementPurger.onPurgeRequest);
  },
  handleHiding: function(data) {
    if (data.hiding)
      block_list_via_css(data.selectors);
  }
});
