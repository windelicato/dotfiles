// Chrome to Safari port
// Author: Michael Gundlach (gundlach@gmail.com)
// License: GPLv3 as part of adblockforchrome.googlecode.com
//          or MIT if GPLv3 conflicts with your code's license.
//
// Porting library to make Chrome extensions work in Safari.
// To use: Add as the first script loaded in your Options page,
// your background page, your Chrome manifest.json, and your
// Safari Info.plist (created by the Extensions Builder).
//
// Then you can use chrome.* APIs as usual, and check the SAFARI
// global boolean variable to see if you're in Safari or Chrome
// for doing browser-specific stuff.  The safari.* APIs will 
// still be available in Safari, and the chrome.* APIs will be
// unchanged in Chrome.

if (typeof SAFARI == "undefined") {
(function() {

// True in Safari, false in Chrome.
SAFARI = (typeof safari !== "undefined");

// Safari 5.0 (533.x.x) with no menu support
LEGACY_SAFARI = SAFARI && (navigator.appVersion.match(/\sSafari\/(\d+)\./) || [null,0])[1] < 534;
// Safari 6.0 implementing removeContentStyleSheet with no bugs
SAFARI6 = SAFARI && (navigator.appVersion.match(/Version\/(\d+)/) || [null, 0])[1] >= 6;

if (SAFARI) {

  var isOnGlobalPage = !!safari.extension.bars;

  // Return the object on which you can add/remove event listeners.
  // If there isn't one, don't explode.
  var listeningContext = function() {
    if (safari.self && safari.self.addEventListener)
      return safari.self;
    if (safari.application && safari.application.addEventListener)
      return safari.application;
    console.log("No add/remove event listener possible at this location!");
    console.trace();
    return { 
      addEventListener: function() {}, 
      removeEventListener: function() {} 
    };
  };
  var listenFor = function(messageName, handler) {
    var listener = function(messageEvent) {
      if (messageEvent.name == messageName)
        handler(messageEvent);
    };
    listeningContext().addEventListener("message", listener, false);
    return listener;
  };
  var removeListener = function(listener) {
    listeningContext().removeEventListener("message", listener, false);
  };
  // Return the object on which you can dispatch messages -- globally, or on the
  // messageEvent if specified.  If there isn't one, don't explode.
  var dispatchContext = function(messageEvent) {
    // Can we dispatch on the messageEvent target?
    var m = messageEvent;
    if (m && m.target && m.target.page && m.target.page.dispatchMessage)
      return m.target.page;
    // Are we in some context where safari.self works, whatever that is?
    var s = safari.self;
    if (s && s.tab && s.tab.dispatchMessage)
      return s.tab;
    // Are we in the global page sending to the active tab?
    var b = (safari.application && safari.application.activeBrowserWindow);
    var p = (b && b.activeTab && b.activeTab.page);
    if (p && p.dispatchMessage)
      return p;
    console.log("No dispatchMessage possible at this location!");
    console.trace();
    return { 
      dispatchMessage: function() {}
    };
  };

  // Track tabs that make requests to the global page, assigning them
  // IDs so we can recognize them later.
  var getTabId = (function() {
    // Tab objects are destroyed when no one has a reference to them,
    // so we keep a list of them, lest our IDs get lost.
    var tabs = [];
    var lastAssignedTabId = 0;
    var theFunction = function(tab) {
      // Clean up closed tabs, to avoid memory bloat.
      tabs = tabs.filter(function(t) { return t.browserWindow != null; });

      if (tab.id == undefined) {
        // New tab
        tab.id = lastAssignedTabId + 1;
        lastAssignedTabId = tab.id;
        tabs.push(tab); // save so it isn't garbage collected, losing our ID.
      }
      return tab.id;
    };
    return theFunction;
  })();


  // Replace the 'chrome' object with a Safari adapter.
  chrome = {
    extension: {
      getBackgroundPage: function() {
        return safari.extension.globalPage.contentWindow;
      },

      getURL: function(path) { 
        return safari.extension.baseURI + path;
      },

      sendRequest: (function() {
        // Where to call .dispatchMessage() when sendRequest is called.
        var dispatchTargets = [];
        if (!isOnGlobalPage) {
          // In a non-global context, the dispatch target is just the local
          // object that lets you call .dispatchMessage().
          dispatchTargets.push(dispatchContext());
        }
        else {
          // In the global context, we must call .dispatchMessage() wherever
          // someone has called .onRequest().  There's no good way to get at
          // them directly, though, so .onRequest calls *us*, so we get access
          // to a messageEvent object that points to their page that we can
          // call .dispatchMessage() upon.
          listenFor("onRequest registration", function(messageEvent) {
            var context = dispatchContext(messageEvent);
            if (dispatchTargets.indexOf(context) == -1)
              dispatchTargets.push(context);
          });
        }

        // Dispatches a request to a list of recipients.  Calls the callback
        // only once, using the first response received from any recipient.
        function theFunction(data, callback) {
          var callbackToken = "callback" + Math.random();

          // Dispatch to each recipient.
          dispatchTargets.forEach(function(target) {
            var message = { data: data, callbackToken: callbackToken };
            target.dispatchMessage("request", message);
          });

          // Listen for a response.  When we get it, call the callback and stop
          // listening.
          var listener = listenFor("response", function(messageEvent) {
            if (messageEvent.message.callbackToken != callbackToken)
              return;
            // Must wrap this call in a timeout to avoid crash, per Safari team
            window.setTimeout(function() {
              removeListener(listener);
            }, 0);
            if (callback)
              callback(messageEvent.message.data);
          });
        }
        return theFunction;
      })(),

      onRequest: {
        addListener: function(handler) {
          // If listening for requests from the global page, we must call the
          // global page so it can get a messageEvent through which to send
          // requests to us.
          if (!isOnGlobalPage)
            dispatchContext().dispatchMessage("onRequest registration", {});

          listenFor("request", function(messageEvent) {
            var request = messageEvent.message.data;

            var sender = {}; // Empty in onRequest in non-global contexts.
            if (isOnGlobalPage) { // But filled with sender data otherwise.
              var id = getTabId(messageEvent.target);
              sender.tab = { id: id, url: messageEvent.target.url };
            }

            var sendResponse = function(dataToSend) {
              var responseMessage = { callbackToken: messageEvent.message.callbackToken, data: dataToSend };
              dispatchContext(messageEvent).dispatchMessage("response", responseMessage);
            };
            handler(request, sender, sendResponse);
          });
        }
      },

      onRequestExternal: {
        addListener: function() {
          // CHROME PORT LIBRARY: onRequestExternal not supported.
        }
      }
    },

    i18n: (function() {

      function syncFetch(file, fn) {
        var xhr = new XMLHttpRequest();
        xhr.open("GET", chrome.extension.getURL(file), false);
        xhr.onreadystatechange = function() {
          if(this.readyState == 4 && this.responseText != "") {
            fn(this.responseText);
          }
        };
        try {
          xhr.send();
        }
        catch (e) {
          // File not found, perhaps
        }
      }

      // Insert substitution args into a localized string.
      function parseString(msgData, args) {
        // If no substitution, just turn $$ into $ and short-circuit.
        if (msgData.placeholders == undefined && args == undefined)
          return msgData.message.replace(/\$\$/g, '$');

        // Substitute a regex while understanding that $$ should be untouched
        function safesub(txt, re, replacement) {
          var dollaRegex = /\$\$/g, dollaSub = "~~~I18N~~:";
          txt = txt.replace(dollaRegex, dollaSub);
          txt = txt.replace(re, replacement);
          // Put back in "$$" ("$$$$" somehow escapes down to "$$")
          var undollaRegex = /~~~I18N~~:/g, undollaSub = "$$$$";
          txt = txt.replace(undollaRegex, undollaSub);
          return txt;
        }

        var $n_re = /\$([1-9])/g;
        var $n_subber = function(_, num) { return args[num - 1]; };

        var placeholders = {};
        // Fill in $N in placeholders
        for (var name in msgData.placeholders) {
          var content = msgData.placeholders[name].content;
          placeholders[name.toLowerCase()] = safesub(content, $n_re, $n_subber);
        }
        // Fill in $N in message
        var message = safesub(msgData.message, $n_re, $n_subber);
        // Fill in $Place_Holder1$ in message
        message = safesub(message, /\$(\w+?)\$/g, function(full, name) {
          var lowered = name.toLowerCase();
          if (lowered in placeholders)
            return placeholders[lowered];
          return full; // e.g. '$FoO$' instead of 'foo'
        });
        // Replace $$ with $
        message = message.replace(/\$\$/g, '$');

        return message;
      }

      var l10nData = undefined;

      var theI18nObject = {
        // chrome.i18n.getMessage() may be used in any extension resource page
        // without any preparation.  But if you want to use it from a content
        // script in Safari, the content script must first run code like this:
        //
        //   get_localization_data_from_global_page_async(function(data) {
        //     chrome.i18n._setL10nData(data);
        //     // now I can call chrome.i18n.getMessage()
        //   });
        //   // I cannot call getMessage() here because the above call
        //   // is asynchronous.
        //
        // The global page will need to receive your request message, call
        // chrome.i18n._getL10nData(), and return its result.
        //
        // We can't avoid this, because the content script can't load
        // l10n data for itself, because it's not allowed to make the xhr
        // call to load the message files from disk.  Sorry :(
        _getL10nData: function() {
          var result = { locales: [] };

          // == Find all locales we might need to pull messages from, in order
          // 1: The user's current locale, converted to match the format of
          //    the _locales directories (e.g. "en-US" becomes "en_US"
          result.locales.push(navigator.language.replace('-', '_'));
          // 2: Perhaps a region-agnostic version of the current locale
          if (navigator.language.length > 2)
            result.locales.push(navigator.language.substring(0, 2));
          // 3: Set English 'en' as default locale
          if (result.locales.indexOf("en") == -1)
            result.locales.push("en");

          // Load all locale files that exist in that list
          result.messages = {};
          for (var i = 0; i < result.locales.length; i++) {
            var locale = result.locales[i];
            var file = "_locales/" + locale + "/messages.json";
            // Doesn't call the callback if file doesn't exist
            syncFetch(file, function(text) {
              result.messages[locale] = JSON.parse(text);
            });
          }

          return result;
        },

        // Manually set the localization data.  You only need to call this
        // if using chrome.i18n.getMessage() from a content script, before
        // the first call.  You must pass the value of _getL10nData(),
        // which can only be called by the global page.
        _setL10nData: function(data) {
          l10nData = data;
        },

        getMessage: function(messageID, args) {
          if (l10nData == undefined) {
            // Assume that we're not in a content script, because content 
            // scripts are supposed to have set l10nData already
            chrome.i18n._setL10nData(chrome.i18n._getL10nData());
          }
          if (typeof args == "string")
            args = [args];
          for (var i = 0; i < l10nData.locales.length; i++) {
            var map = l10nData.messages[l10nData.locales[i]];
            // We must have the locale, and the locale must have the message
            if (map && messageID in map)
              return parseString(map[messageID], args);
          }
          return "";
        }
      };

      return theI18nObject;
    })()

  };
}

})(); } // end if (typeof SAFARI == "undefined") { (function() {
