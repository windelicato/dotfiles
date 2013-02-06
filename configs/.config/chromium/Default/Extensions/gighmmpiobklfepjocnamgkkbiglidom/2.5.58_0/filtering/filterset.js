// Filter objects representing the given filter text.
function FilterSet() {
  // Map from domain (e.g. 'mail.google.com', 'google.com', or special-case
  // 'global') to list of filters that specify inclusion on that domain.
  // E.g. /f/$domain=sub.foo.com,bar.com will appear in items['sub.foo.com']
  // and items['bar.com'].
  this.items = { 'global': [] };
  // Map from domain to set of filter ids that specify exclusion on that domain.
  // Each filter will also appear in this.items at least once.
  // Examples:
  //   /f/$domain=~foo.com,~bar.com would appear in
  //     items['global'], exclude['foo.com'], exclude['bar.com']
  //   /f/$domain=foo.com,~sub.foo.com would appear in
  //     items['foo.com'], exclude['sub.foo.com']
  this.exclude = {};
}


// Construct a FilterSet from the Filters that are the values in the |data|
// object.  All filters should be the same type (whitelisting PatternFilters,
// blocking PatternFilters, or SelectorFilters.)
FilterSet.fromFilters = function(data) {
  var result = new FilterSet();

  for (var _ in data) {
    var filter = data[_];

    for (var d in filter._domains.has) {
      if (filter._domains.has[d]) {
        var key = (d === DomainSet.ALL ? 'global' : d);
        setDefault(result.items, key, []).push(filter);
      }
      else if (d !== DomainSet.ALL)
        setDefault(result.exclude, d, {})[filter.id] = true;
    }
  }

  return result;
}

FilterSet.prototype = {
  // Return a new FilterSet containing the subset of this FilterSet's entries
  // which relate to the given domain or any of its superdomains.  E.g.
  // sub.foo.com will get items['global', 'foo.com', 'sub.foo.com'] and
  // exclude['foo.com', 'sub.foo.com'].
  _viewFor: function(domain) {
    var result = new FilterSet();
    result.items['global'] = this.items['global'];
    for (var nextDomain in DomainSet.domainAndParents(domain)) {
      if (this.items[nextDomain])
        result.items[nextDomain] = this.items[nextDomain];
      if (this.exclude[nextDomain])
        result.exclude[nextDomain] = this.exclude[nextDomain];
    }
    return result;
  },

  // Get a list of all Filter objects that should be tested on the given
  // domain, and return it with the given map function applied. This function
  // is for hiding rules only
  filtersFor: function(domain) {
    var limited = this._viewFor(domain);
    var data = {};
    // data = set(limited.items)
    for (var subdomain in limited.items) {
      var entry = limited.items[subdomain];
      for (var i = 0; i < entry.length; i++) {
        var filter = entry[i];
        data[filter.id] = filter;
      }
    }
    // data -= limited.exclude
    for (var subdomain in limited.exclude) {
      for (var filterId in limited.exclude[subdomain]) {
        delete data[filterId];
      }
    }
    var result = [];
    for (var k in data)
      result.push(data[k].selector);
    return result;
  },

  // Return the filter that matches this url+elementType on this frameDomain:
  // the filter in a relevant entry in this.items who is not also in a 
  // relevant entry in this.exclude.
  // isThirdParty: true if url and frameDomain have different origins.
  matches: function(url, elementType, frameDomain, isThirdParty) {
    var limited = this._viewFor(frameDomain);
    for (var k in limited.items) {
      var entry = limited.items[k];
      for (var i = 0; i < entry.length; i++) {
        var filter = entry[i];
        if (!filter.matches(url, elementType, isThirdParty))
          continue; // no match
        // Maybe filter shouldn't match because it is excluded on our domain?
        var excluded = false;
        for (var k2 in limited.exclude) {
          if (limited.exclude[k2][filter.id]) {
            excluded = true;
            break;
          }
        }
        if (!excluded)
          return filter;
      }
    }

    return null;
  }
};


BlockingFilterSet = function(patternFilterSet, whitelistFilterSet) {
  this.pattern = patternFilterSet;
  this.whitelist = whitelistFilterSet;

  // Caches results for this.matches() 
  this._matchCache = {};
}

// Checks if the two domains have the same origin
// Inputs: the two domains
// Returns: true if third-party, false otherwise
BlockingFilterSet.checkThirdParty = function(domain1, domain2) {
  var match1 = parseUri.secondLevelDomainOnly(domain1, false);
  var match2 = parseUri.secondLevelDomainOnly(domain2, false);
  return (match1 !== match2);
}

BlockingFilterSet.prototype = {
  // True if the url is blocked by this filterset.
  // Inputs:
  //   url:string - The URL of the resource to possibly block
  //   elementType:ElementType - the type of element that is requesting the 
  //                             resource
  //   frameDomain:string - domain of the frame on which the element resides
  //   returnFilter?:bool - see Returns
  // Returns:
  //   if returnFilter is true:
  //       text of matching pattern/whitelist filter, null if no match
  //   if returnFilter is false:
  //       true if the resource should be blocked, false otherwise
  matches: function(url, elementType, frameDomain, returnFilter) {
    var urlDomain = parseUri(url).hostname;
    var isThirdParty = BlockingFilterSet.checkThirdParty(urlDomain, frameDomain);

    // matchCache approach taken from ABP
    var key = url + " " + elementType + " " + isThirdParty;
    if (key in this._matchCache)
      return this._matchCache[key];

    var match = this.whitelist.matches(url, elementType, frameDomain, isThirdParty);
    if (match) {
      log(frameDomain, ": whitelist rule", match._rule, "exempts url", url);
      this._matchCache[key] = (returnFilter ? match._text : false);
      return this._matchCache[key];
    }
    match = this.pattern.matches(url, elementType, frameDomain, isThirdParty);
    if (match) {
      log(frameDomain, ": matched", match._rule, "to url", url);
      this._matchCache[key] = (returnFilter ? match._text: true);
      return this._matchCache[key];
    }
    this._matchCache[key] = false;
    return this._matchCache[key];
  },
}
