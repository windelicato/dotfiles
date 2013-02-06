// Set to true to get noisier console.log statements
VERBOSE_DEBUG = false;

// Issue 6614: Don't run in a frame, to avoid manipulation by websites.
if (window.location.origin + "/" === chrome.extension.getURL("")) {
  // above line avoids content scripts making their host page break frames
  if (window.top !== window)
    window.location.replace("about:blank");
}

// Run a function on the background page.
// Inputs (positional):
//   first, a string - the name of the function to call
//   then, any arguments to pass to the function (optional)
//   then, a callback:function(return_value:any) (optional)
BGcall = function() {
  var args = [];
  for (var i=0; i < arguments.length; i++)
    args.push(arguments[i]);
  var fn = args.shift();
  var has_callback = (typeof args[args.length - 1] == "function");
  var callback = (has_callback ? args.pop() : function() {});
  chrome.extension.sendRequest({command: "call", fn:fn, args:args}, callback);
};

// Enabled in adblock_start_common.js and background.js if the user wants
logging = function(enabled) {
  if (enabled) {
    log = function() {
      if (VERBOSE_DEBUG || arguments[0] != '[DEBUG]') // comment out for verbosity
        console.log.apply(console, arguments);
    };
    logGroup = function() { console.group.apply(console, arguments); };
    logGroupEnd = function() { console.groupEnd(); };
  }
  else {
    log = logGroup = logGroupEnd = function() {};
  }
};
logging(false); // disabled by default

// Behaves very similarly to $.ready() but does not require jQuery.
onReady = function(callback) {
  if (document.readyState === "complete")
    window.setTimeout(callback, 0);
  else
    window.addEventListener("load", callback, false);
};

translate = function(messageID, args) {
  return chrome.i18n.getMessage(messageID, args);
};

localizePage = function() {
  //translate a page into the users language
  $("[i18n]:not(.i18n-replaced)").each(function() {
    $(this).html(translate($(this).attr("i18n")));
  });
  $("[i18n_value]:not(.i18n-replaced)").each(function() {
    $(this).val(translate($(this).attr("i18n_value")));
  });
  $("[i18n_title]:not(.i18n-replaced)").each(function() {
    $(this).attr("title", translate($(this).attr("i18n_title")));
  });
  $("[i18n_placeholder]:not(.i18n-replaced)").each(function() {
    $(this).attr("placeholder", translate($(this).attr("i18n_placeholder")));
  });
  $("[i18n_replacement_el]:not(.i18n-replaced)").each(function() {
    // Replace a dummy <a/> inside of localized text with a real element.
    // Give the real element the same text as the dummy link.
    var dummy_link = $("a", this);
    var text = dummy_link.text();
    var real_el = $("#" + $(this).attr("i18n_replacement_el"));
    real_el.text(text).val(text).replaceAll(dummy_link);
    // If localizePage is run again, don't let the [i18n] code above
    // clobber our work
    $(this).addClass("i18n-replaced");
  });
};

// Parse a URL. Based upon http://blog.stevenlevithan.com/archives/parseuri
// parseUri 1.2.2, (c) Steven Levithan <stevenlevithan.com>, MIT License
// Inputs: url: the URL you want to parse
// Outputs: object containing all parts of |url| as attributes
parseUri = function(url) {
  var matches = /^(([^:]+(?::|$))(?:(?:\w+:)?\/\/)?(?:[^:@\/]*(?::[^:@\/]*)?@)?(([^:\/?#]*)(?::(\d*))?))((?:[^?#\/]*\/)*[^?#]*)(\?[^#]*)?(\#.*)?/.exec(url);
  // The key values are identical to the JS location object values for that key
  var keys = ["href", "origin", "protocol", "host", "hostname", "port",
              "pathname", "search", "hash"];
  var uri = {};
  for (var i=0; i<keys.length; i++)
    uri[keys[i]] = matches[i] || "";
  return uri;
};
// Parses the search part of a URL into an key: value object.
// e.g., ?hello=world&ext=adblock would become {hello:"world", ext:"adblock"}
// Inputs: search: the search query of a URL. Must have &-separated values.
parseUri.parseSearch = function(search) {
  // Fails if a key exists twice (e.g., ?a=foo&a=bar would return {a:"bar"}
  var queryKeys = {};
  search.replace(/(?:^\?|&)([^&=]*)=?([^&]*)/g, function () {
    if (arguments[1]) queryKeys[arguments[1]] = unescape(arguments[2]);
  });
  return queryKeys;
};
// Strip third+ level domain names from the domain and return the result.
// Inputs: domain: the domain that should be parsed
//         keepDot: true if trailing dots should be preserved in the domain
// Returns: the parsed domain
parseUri.secondLevelDomainOnly = function(domain, keepDot) {
  var match = domain.match(/([^\.]+\.(?:co\.)?[^\.]+)\.?$/) || [domain, domain];
  return match[keepDot ? 0 : 1].toLowerCase();
};

// TODO: move back into background.js since Safari can't use this
// anywhere but in the background.  Do it after merging 6101 and 6238
// and 5912 to avoid merge conflicts.
// Inputs: key:string.
// Returns value if key exists, else undefined.
storage_get = function(key) {
  var store = (window.SAFARI ? safari.extension.settings : localStorage);
  var json = store.getItem(key);
  if (json == null)
    return undefined;
  try {
    return JSON.parse(json);
  } catch (e) {
    log("Couldn't parse json for " + key);
    return undefined;
  }
};

// Inputs: key:string, value:object.
// If value === undefined, removes key from storage.
// Returns undefined.
storage_set = function(key, value) {
  var store = (window.SAFARI ? safari.extension.settings : localStorage);
  if (value === undefined) {
    store.removeItem(key);
    return;
  }
  try {
    store.setItem(key, JSON.stringify(value));
  } catch (ex) {
    // Safari throws this error for all writes in Private Browsing mode.
    // TODO: deal with the Safari case more gracefully.
    if (ex.name == "QUOTA_EXCEEDED_ERR" && !SAFARI) {
      alert(translate("storage_quota_exceeded"));
      openTab("options/index.html#ui-tabs-2");
    }
  }
};

// Return obj[value], first setting it to |defaultValue| if it is undefined.
setDefault = function(obj, value, defaultValue) {
  if (obj[value] === undefined)
    obj[value] = defaultValue;
  return obj[value];
};
