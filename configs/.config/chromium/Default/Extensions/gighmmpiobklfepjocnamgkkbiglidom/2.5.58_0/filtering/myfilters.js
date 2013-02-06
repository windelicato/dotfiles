// Requires jquery and must be on a page with access to the background page

// MyFilters class manages subscriptions and the FilterSet.

// Constructor: merge the stored subscription data and the total possible
// list of subscriptions into this._subscriptions.  Store to disk.
// Inputs: none.
function MyFilters() {
  this._subscriptions = storage_get('filter_lists');
  this._official_options = this._make_subscription_options();
  
  var newUser = !this._subscriptions;
  if (newUser) {
    // Brand new user. Install some filters for them.
    this._subscriptions = this._load_default_subscriptions();
  }

  for (var id in this._subscriptions) {
    // Delete unsubscribed ex-official lists.
    if (!this._official_options[id] && !this._subscriptions[id].user_submitted
        && !this._subscriptions[id].subscribed) {
      delete this._subscriptions[id];
    }
    // Convert subscribed ex-official lists into user-submitted lists.
    // Convert subscribed ex-user-submitted lists into official lists.
    else {
      this._subscriptions[id].user_submitted = !this._official_options[id];
    }
  }

  // Use the stored properties, and only add any new properties and/or lists
  // if they didn't exist in this._subscriptions
  for (var id in this._official_options) {
    if (!this._subscriptions[id])
      this._subscriptions[id] = {};
    var sub = this._subscriptions[id];
    var official = this._official_options[id];

    sub.initialUrl = sub.initialUrl || official.url;
    sub.url = sub.url || official.url;
    if (sub.initialUrl != official.url) {
      // The official URL was changed. Use it. In case of a redirect, this
      // doesn't happen as only sub.url is changed, not sub.initialUrl.
      sub.initialUrl = official.url;
      sub.url = official.url;
    }
    sub.requiresList = official.requiresList;
    sub.subscribed = sub.subscribed || false;
  }

  // Build the filter list
  this._onSubscriptionChange(true);

  // On startup and then every hour, check if a list is out of date and has to
  // be updated
  var that = this;
  if (newUser) 
    this.checkFilterUpdates();
  else
    idleHandler.scheduleItemOnce(
      function() { 
        that.checkFilterUpdates();
      },
      60
    );

  window.setInterval(
    function() { 
      idleHandler.scheduleItemOnce(function() {
        that.checkFilterUpdates();
      });
    }, 
    60 * 60 * 1000
  );
}

// When a subscription property changes, this function stores it
// Inputs: rebuild? boolean, true if the filterset should be rebuilt
MyFilters.prototype._onSubscriptionChange = function(rebuild) {
  storage_set('filter_lists', this._subscriptions);

  // The only reasons to (re)build the filter set are
  // - when AdBlock starts
  // - when a filter list text is changed ([un]subscribed or updated a list)
  if (rebuild)
    this.rebuild();

  chrome.extension.sendRequest({command: "filters_updated"});
}

// get filters that are defined in the extension
MyFilters.prototype.getExtensionFilters = function(settings) {
  //Exclude google search results ads if the user has checked that option
  var texts = [];
  if (settings.show_google_search_text_ads) {
    // Standard search
    texts.push("@@||google.*/search?$elemhide");
    // Google Instant: go to google.com, type 'hotel' and don't press Enter
    texts.push("@@||www.google.*/|$elemhide");
    // Google Instant: open a Chrome tab, type 'hotel' and don't press Enter
    texts.push("@@||google.*/webhp?*sourceid=*instant&$elemhide");
  }
  if (settings.whitelist_hulu_ads) {
    // Issue 7178: FilterNormalizer removes EasyList's too-broad Hulu whitelist
    // entries.  If the user enables whitelist_hulu_ads, just add them back.
    // This workaround can be removed when EasyList changes its Hulu strategy.
    texts.push("@@||ads.hulu.com/published/*.flv");
    texts.push("@@||ads.hulu.com/published/*.mp4");
    texts.push("@@||ll.a.hulu.com/published/*.flv");
    texts.push("@@||ll.a.hulu.com/published/*.mp4");
  }
  return texts;
};

// Rebuild filters based on the current settings and subscriptions.
MyFilters.prototype.rebuild = function() {
  var texts = [];
  for (var id in this._subscriptions)
    if (this._subscriptions[id].subscribed)
      texts.push(this._subscriptions[id].text);

  // Include custom filters.
  var customfilters = get_custom_filters_text(); // from background
  if (customfilters)
    texts.push(FilterNormalizer.normalizeList(customfilters));

  texts = texts.concat(this.getExtensionFilters(get_settings()));

  texts = texts.join('\n').split('\n');

  // Remove duplicates and empties.
  var unique = {};
  for (var i = 0; i < texts.length; i++)
    unique[texts[i]] = 1;
  delete unique[''];

  var filters = { hidingUnmerged: [], hiding: {}, exclude: {},
                  pattern: {}, whitelist: {} };
  for (var text in unique) {
    var filter = Filter.fromText(text);
    if (Filter.isSelectorExcludeFilter(text))
      setDefault(filters.exclude, filter.selector, []).push(filter);
    else if (Filter.isSelectorFilter(text))
      filters.hidingUnmerged.push(filter);
    else if (Filter.isWhitelistFilter(text))
      filters.whitelist[filter.id] = filter;
    else
      filters.pattern[filter.id] = filter;
  }
  for (var i = 0; i < filters.hidingUnmerged.length; i++) {
    filter = filters.hidingUnmerged[i];
    var hider = SelectorFilter.merge(filter, filters.exclude[filter.selector]);
    filters.hiding[hider.id] = hider;
  }

  if (SAFARI6) {
    var new_hiding = get_settings().new_safari_hiding;
    if (new_hiding && !this.styleSheetRegistrar) {
      logGroup("Creating StyleSheetRegistrar");
      this.styleSheetRegistrar = new StyleSheetRegistrar();
      logGroupEnd();
    }
    else if (!new_hiding && this.styleSheetRegistrar) {
      logGroup("Deleting StyleSheetRegistrar");
      this.styleSheetRegistrar.pause(true);
      delete this.styleSheetRegistrar;
      logGroupEnd();
    }
  }

  if (this.styleSheetRegistrar)
    this.styleSheetRegistrar.register(filters.hiding);
  else
    this.hiding = FilterSet.fromFilters(filters.hiding);

  this.blocking = new BlockingFilterSet(
    FilterSet.fromFilters(filters.pattern), 
    FilterSet.fromFilters(filters.whitelist)
  );
  handlerBehaviorChanged(); // defined in background
  
  // After 90 seconds, delete the cache. That way the cache is available when
  // rebuilding multiple times in a row (when multiple lists have to update at
  // the same time), but we save memory during all other times.
  window.setTimeout(function() {
    Filter._cache = {};
  }, 90000);
}

// Change a property of a subscription or check if it has to be updated
// Inputs: id: the id of the subscription to change
//         subData: object containing all data that should be changed
//         forceFetch: if the subscriptions have to be fetched again forced
MyFilters.prototype.changeSubscription = function(id, subData, forceFetch) {
  var subscribeRequiredListToo = false;
  var listDidntExistBefore = false;

  // Working with an unknown list: create the list entry
  if (!this._subscriptions[id]) {
    id = this.customToDefaultId(id);
    if (/^url\:.*/.test(id)) {
      listDidntExistBefore = true;
      this._subscriptions[id] = {
        user_submitted: true,
        url: id.substr(4)
      };
    }
    subscribeRequiredListToo = true;
  }

  // Subscribing to a well known list should also subscribe to a required list
  if (!this._subscriptions[id].subscribed && subData.subscribed)
    subscribeRequiredListToo = true;

  // Apply all changes from subData
  for (var property in subData)
    if (subData[property] !== undefined)
      this._subscriptions[id][property] = subData[property];

  // Check if the required list is a well known list, but only if it is changed
  if (subData.requiresList)
    this._subscriptions[id].requiresList = 
                   this.customToDefaultId(this._subscriptions[id].requiresList);

  if (forceFetch)
    delete this._subscriptions[id].last_modified;

  if (this._subscriptions[id].subscribed) {
    // Check if the list has to be updated
    function out_of_date(subscription) {
      if (forceFetch) return true;
      var millis = Date.now() - subscription.last_update;
      return (millis > 1000 * 60 * 60 * subscription.expiresAfterHours);
    }

    if (!this._subscriptions[id].text || out_of_date(this._subscriptions[id]))
      this.fetch_and_update(id, listDidntExistBefore);

  } else {
    // If unsubscribed, remove some properties
    delete this._subscriptions[id].text;
    delete this._subscriptions[id].last_update;
    delete this._subscriptions[id].expiresAfterHours;
    delete this._subscriptions[id].last_update_failed;
    delete this._subscriptions[id].last_modified;
    if (this._subscriptions[id].deleteMe)
      delete this._subscriptions[id];
  }

  // Notify of change.  If we subscribed, we rebuilt above; so we
  // only force a rebuild if we unsubscribed.
  this._onSubscriptionChange(subData.subscribed == false);

  // Subscribe to a required list if nessecary
  if (subscribeRequiredListToo && this._subscriptions[id].requiresList)
    this.changeSubscription(this._subscriptions[id].requiresList, {subscribed:true});
}

// Fetch a filter list and parse it
// id:        the id of the list
// isNewList: true when the list is completely new and must succeed or 
//            otherwise it'll be deleted.
MyFilters.prototype.fetch_and_update = function(id, isNewList) {
  var url = this._subscriptions[id].url;
  var that = this;
  function onError() {
    that._subscriptions[id].last_update_failed = true;
    that._onSubscriptionChange();
    if (isNewList) {
      // Delete the list. The user subscribed to an invalid list URL.
      // Delay it a bit, so subscribe.html can first finish it's async call to
      // see if the subscription succeeded or not. Otherwise it assumes it was
      // a well-known subscription and will report a succesfull fetch
      window.setTimeout(function() {
        that.changeSubscription(id, {subscribed: false, deleteMe: true});
      }, 500);
    }
  }
  $.ajax({
    url: url,
    cache: false,
    headers: {
      "Accept": "text/plain",
      "If-Modified-Since": this._subscriptions[id].last_modified || undefined
    },
    success: function(text, status, xhr) {
      // In case the subscription disappeared while we were out
      if (!that._subscriptions[id] || 
          !that._subscriptions[id].subscribed)
        return;

      // Sometimes text is "". Happens sometimes.  Weird, I know.
      // Every legit list starts with a comment.
      if (status == "notmodified") {
        log("List not modified " + url);
        that._updateSubscriptionText(id, that._subscriptions[id].text);
        that._onSubscriptionChange(true);
      } else if (text && text.length != 0 && Filter.isComment(text.trim())) {
        log("Fetched " + url);
        that._updateSubscriptionText(id, text, xhr);
        that._onSubscriptionChange(true);
      } else {
        log("Fetched, but invalid list " + url);
        onError();
      }
    },
    error: function() {
      if (that._subscriptions[id])
        onError();
      log("Error fetching " + url);
    }
  });
}

// Record that subscription_id is subscribed, was updated now, and has
// the given text.  Requires that this._subscriptions[subscription_id] exists.
// The xhr variable can be used to search the response headers
MyFilters.prototype._updateSubscriptionText = function(id, text, xhr) {
  this._subscriptions[id].last_update = Date.now();
  delete this._subscriptions[id].last_update_failed;

  // In case the resource wasn't modified, there is no need to reparse this.
  // xhr isn't send in this case. Do reparse .text, in case we had some update
  // which modified the checks in filternormalizer.js.
  if (xhr) {
    // Store the last time a resource was modified on the server, so we won't re-
    // fetch if it wasn't modified. It is null if the server doesn't support this.
    this._subscriptions[id].last_modified = xhr.getResponseHeader("Last-Modified");
    // Record how many hours until we need to update the subscription text. This
    // can be specified in the file. Defaults to 120.
    this._subscriptions[id].expiresAfterHours = 120;
    var checkLines = text.split('\n', 15); //15 lines should be enough
    var expiresRegex = /(?:expires\:|expires\ after\ )\ *(\d+)\ ?(h?)/i;
    var redirectRegex = /(?:redirect\:|redirects\ to\ )\ *(https?\:\/\/\S+)/i;
    for (var i = 0; i < checkLines.length; i++) {
      if (!Filter.isComment(checkLines[i]))
        continue;
      var match = checkLines[i].match(redirectRegex);
      if (match) {
        this._subscriptions[id].url = match[1]; //assuming the URL is always correct
        // Force an update.  Even if our refetch below fails we'll have to 
        // fetch the new URL in the future until it succeeds.
        this._subscriptions[id].last_update = 0;
      }
      match = checkLines[i].match(expiresRegex);
      if (match && parseInt(match[1], 10)) {
        var hours = parseInt(match[1], 10) * (match[2] == "h" ? 1 : 24);
        this._subscriptions[id].expiresAfterHours = Math.min(hours, 21*24); // 3 week maximum
      }
    }
  }

  this._subscriptions[id].text = FilterNormalizer.normalizeList(text);

  // The url changed. Simply refetch...
  if (this._subscriptions[id].last_update === 0)
    this.changeSubscription(id, {}, true);
}

// Checks if subscriptions have to be updated
// Inputs: force? (boolean), true if every filter has to be updated
MyFilters.prototype.checkFilterUpdates = function(force) {
  for (var id in this._subscriptions) {
    if (this._subscriptions[id].subscribed) {
      this.changeSubscription(id, {}, force);
    }
  }
}

// Checks if a custom id is of a known list
// Inputs: id: the list id to compare
// Returns the id that should be used
MyFilters.prototype.customToDefaultId = function(id) {
  var urlOfCustomList = id.substr(4);
  for (var defaultList in this._official_options)
    if (this._official_options[defaultList].url == urlOfCustomList)
      return defaultList;
  // We use a mirror of EasyList. However, to prevent users from getting
  // subscribed to both the mirror as the official one, have this check...
  if (urlOfCustomList == "https://easylist-downloads.adblockplus.org/easylist.txt")
    return "easylist";
  return id;
}

// If the user wasn't subscribed to any lists, subscribe to
// EasyList, AdBlock custom and (if any) a localized subscription
// Inputs: none.
// Returns an object containing the subscribed lists
MyFilters.prototype._load_default_subscriptions = function() {
  var result = {};

  // Returns the ID of the list appropriate for the user's locale, or ''
  function listIdForThisLocale() {
    var language = navigator.language.match(/^([a-z]+).*/i)[1];
    switch(language) {
      case 'bg': return 'easylist_plus_bulgarian';
      case 'cs': return 'czech';
      case 'cu': return 'easylist_plus_bulgarian';
      case 'da': return 'danish';
      case 'de': return 'easylist_plus_german';
      case 'es': return 'easylist_plus_spanish';
      case 'el': return 'easylist_plus_greek';
      case 'fi': return 'easylist_plus_finnish';
      case 'fr': return 'easylist_plus_french';
      case 'he': return 'israeli';
      case 'hu': return 'hungarian';
      case 'it': return 'italian';
      case 'ja': return 'japanese';
      case 'ko': return 'easylist_plun_korean';
      case 'nl': return 'dutch';
      case 'pl': return 'easylist_plus_polish';
      case 'ro': return 'easylist_plus_romanian';
      case 'ru': return 'russian';
      case 'uk': return 'russian';
      case 'zh': return 'chinese';
      default: return '';
    }
  }

  //Update will be done immediately after this function returns
  result["adblock_custom"] = { subscribed: true };
  result["easylist"] = { subscribed: true };
  
  var list_for_lang = listIdForThisLocale();
  if (list_for_lang)
    result[list_for_lang] = { subscribed: true };

  return result;
}

// Used to create the list of default subscriptions
// Called when MyFilters is created.
// Returns: that list
MyFilters.prototype._make_subscription_options = function() {
  // When modifying a list, IDs mustn't change!
  return {
    "adblock_custom": { // AdBlock custom filters
      url: "https://chromeadblock.com/filters/adblock_custom.txt",
    },
    "easylist": { // EasyList
      url: "http://adblockplus.mozdev.org/easylist/easylist.txt"
    },
    "easylist_plus_bulgarian": { // Additional Bulgarian filters
      url: "http://stanev.org/abp/adblock_bg.txt",
      requiresList: "easylist",
    },
    "dutch": { // Additional Dutch filters
      url: "https://dutchadblockfilters.googlecode.com/svn/trunk/AdBlock_Dutch_hide.txt",
      requiresList: "easylist",
    },
    "easylist_plus_finnish": { // Additional Finnish filters
      url: "http://download.wiltteri.net/wiltteri.txt",
      requiresList: "easylist",
    },
    "easylist_plus_french": { // Additional French filters
      url: "https://listefr-adblock.googlecode.com/hg/liste_fr.txt",
      requiresList: "easylist",
    },
    "easylist_plus_german": { // Additional German filters
      url: "http://adblockplus.mozdev.org/easylist/easylistgermany.txt",
      requiresList: "easylist",
    },
    "easylist_plus_greek": { // Additional Greek filters
      url: "http://www.void.gr/kargig/void-gr-filters.txt",
      requiresList: "easylist",
    },
    "easylist_plus_polish": { // Additional Polish filters
      url: "http://adblocklist.org/adblock-pxf-polish.txt",
      requiresList: "easylist",
    },
    "easylist_plus_romanian": { // Additional Romanian filters
      url: "http://www.zoso.ro/pages/rolist.txt",
      requiresList: "easylist",
    },
    "russian": { // Additional Russian filters
      url: "https://ruadlist.googlecode.com/hg/advblock.txt",
      requiresList: "easylist",
    },
    "chinese": { // Additional Chinese filters
      url: "https://adblock-chinalist.googlecode.com/svn/trunk/adblock.txt",
      requiresList: "easylist",
    },
    "czech": { // Czech filters
      url: "http://adblock.dajbych.net/adblock.txt",
    },
    "danish": { // Danish filters
      url: "http://adblock.schack.dk/block.txt",
    },
    "hungarian": { // Hungarian filters
      url: "http://pete.teamlupus.hu/hufilter.txt",
    },
    "israeli": { // Israeli filters
      url: "https://secure.fanboy.co.nz/israelilist/IsraelList.txt",
    },
    "italian": { // Italian filters
      url: "http://mozilla.gfsolone.com/filtri.txt",
    },
    "japanese": { // Japanese filters
      url: "https://secure.fanboy.co.nz/fanboy-japanese.txt",
    },
    "easylist_plun_korean": {  // Korean filters
      url: "https://secure.fanboy.co.nz/fanboy-korean.txt",
    },
    "easylist_plus_spanish": {  // Spanish filters
      url: "http://abp.mozilla-hispano.org/nauscopio/filtros.txt",
    },
    "easyprivacy": { // EasyPrivacy
      url: "https://easylist-downloads.adblockplus.org/easyprivacy.txt",
    },
  };
}

/* subscription properties:
url (string): url of subscription
initialUrl (string): the hardcoded url. Same as .url except when redirected
user_submitted (bool): submitted by the user or not
requiresList (string): id of a list required for this list
subscribed (bool): if you are subscribed to the list or not
last_update (date): time of the last succesfull update
last_modified (string): time of the last change on the server
last_update_failed (bool): true if the last update attempt failed
text (string): the filters of the subscription
expiresAfterHours (int): the time after which the subscription expires
deleteMe (bool): if the subscription has to be deleted
*/
