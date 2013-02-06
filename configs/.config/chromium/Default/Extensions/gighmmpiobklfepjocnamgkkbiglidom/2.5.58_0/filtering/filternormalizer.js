// Converts non-standard filters to a standard format, and removes
// invalid filters.
var FilterNormalizer = {

  // Normalize a set of filters.
  // Remove broken filters, useless comments and unsupported things.
  // Input: text:string filter strings separated by '\n'
  //        keepComments:boolean if true, comments will not be removed
  // Returns: filter strings separated by '\n' with invalid filters
  //          removed or modified
  normalizeList: function(text, keepComments) {
    var lines = text.split('\n');
    delete text;
    var result = [];
    var ignoredFilterCount = 0;
    for (var i=0; i<lines.length; i++) {
      try {
        var newfilter = FilterNormalizer.normalizeLine(lines[i]);
        if (newfilter)
          result.push(newfilter);
        else if (newfilter !== false)
          ignoredFilterCount++;
        else if (keepComments)
          result.push(lines[i]);
      } catch (ex) {
        log("Filter '" + lines[i] + "' could not be parsed: " + ex);
        ignoredFilterCount++;
      }
    }
    if (ignoredFilterCount)
      log('Ignoring ' + ignoredFilterCount + ' rule(s)');
    return result.join('\n') + '\n';
  },

  // Normalize a single filter.
  // Input: filter:string a single filter
  // Return: normalized filter string if the filter is valid, null if the filter
  //         will be ignored or false if it isn't supposed to be a filter.
  // Throws: exception if filter could not be parsed.
  //
  // Note that 'Expires' comments are considered valid comments that
  // need retention, because they carry information.
  normalizeLine: function(filter) {
    // Some rules are separated by \r\n; and hey, some rules may
    // have leading or trailing whitespace for some reason.
    filter = filter.replace(/\r$/, '').trim();

    // Remove comment/empty filters.
    if (Filter.isComment(filter))
        return false;

    // Convert old-style hiding rules to new-style.
    if (/#[\*a-z0-9_\-]*(\(|$)/.test(filter) && !/\#\@?\#./.test(filter)) {
      // Throws exception if unparseable.
      var oldFilter = filter;
      filter = FilterNormalizer._old_style_hiding_to_new(filter);
      log('Converted ' + oldFilter + ' to ' + filter);
    }

    // If it is a hiding rule...
    if (Filter.isSelectorFilter(filter)) {
      // The filter must be of a correct syntax

      try {
        // Throws if the filter is invalid...
        var selectorPart = filter.replace(/^.*?\#\@?\#/, '');
        if (document.querySelector(selectorPart + ',html').length === 0)
          throw "Causes other filters to fail";
      } catch(ex) {
        // ...however, the thing it throws is not human-readable. This is.
        throw "Invalid CSS selector syntax";
      }

      // On a few sites, we have to ignore [style] rules.
      // Affects Chrome (crbug 68705) and Safari (issue 6225).
      // Don't exclude the sites unless the filter would apply to them, or
      // loading the site will hang in Safari 6 while Safari creates a bunch of
      // one-off style sheets (issue 7356).
      if (/style([\^\$\*]?=|\])/.test(filter)) {
        var excludedDomains = ["mail.google.com", "mail.yahoo.com"];
        filter = FilterNormalizer._ensureExcluded(filter, excludedDomains);
      }

      var parsedFilter = new SelectorFilter(filter);

    } else { // If it is a blocking rule...
      var parsedFilter = PatternFilter.fromText(filter); // throws if invalid
      var types = parsedFilter._allowedElementTypes;

      var whitelistOptions = (ElementTypes.document | ElementTypes.elemhide);
      var hasWhitelistOptions = types & whitelistOptions;
      if (!Filter.isWhitelistFilter(filter) && hasWhitelistOptions)
        throw "$document and $elemhide may only be used on whitelist filters";

      // Issue 7178
      if (!SAFARI && 
          /^\@\@\|\|(ads|ll\.a)\.hulu\.com\/published\/\*\.(flv|mp4)$/.test(filter)) {
        return null; // background.js implements this rule more specifically
      }
    
      // In Safari, ignore rules with only Chrome-specific types (no-ops).
      if (SAFARI && types === (types & ElementTypes.CHROMEONLY))
        return null;
    }

    // Ignore filters whose domains aren't formatted properly.
    FilterNormalizer.verifyDomains(parsedFilter._domains);

    // Ensure filter doesn't break AdBlock
    FilterNormalizer._checkForObjectProperty(filter);

    // Nothing's wrong with the filter.
    return filter;
  },

  // Return |selectorFilterText| modified if necessary so that it applies to no
  // domain in the |excludedDomains| list.
  // Throws if |selectorFilterText| is not a valid filter.
  // Example: ("a.com##div", ["sub.a.com", "b.com"]) -> "a.com,~sub.a.com##div"
  _ensureExcluded: function(selectorFilterText, excludedDomains) {
    var text = selectorFilterText;
    var filter = new SelectorFilter(text);
    var mustExclude = excludedDomains.filter(function(domain) {
      return filter._domains._computedHas(domain);
    });
    if (mustExclude.length > 0) {
      var toPrepend = "~" + mustExclude.join(",~");
      if (text[0] != "#") toPrepend += ",";
      text = toPrepend + text;
    }
    return text;
  },

  // Convert an old-style hiding rule to a new one.
  // Input: filter:string old-style filter
  // Returns: string new-style filter
  // Throws: exception if filter is unparseable.
  _old_style_hiding_to_new: function(filter) {
    // Old-style is domain#node(attr=value) or domain#node(attr)
    // domain and node are optional, and there can be many () parts.
    filter = filter.replace('#', '##');
    var parts = filter.split('##'); // -> [domain, rule]
    var domain = parts[0];
    var rule = parts[1];

    // Make sure the rule has only the following two things:
    // 1. a node -- this is optional and must be '*' or alphanumeric
    // 2. a series of ()-delimited arbitrary strings -- also optional
    //    the ()s can't be empty, and can't start with '='
    if (rule.length == 0 ||
        !/^(?:\*|[a-z0-9\-_]*)(?:\([^=][^\)]*?\))*$/i.test(rule))
      throw "bad selector filter";

    var first_segment = rule.indexOf('(');

    if (first_segment == -1)
      return domain + '##' + rule;

    var node = rule.substring(0, first_segment);
    var segments = rule.substring(first_segment);

    // turn all (foo) groups into [foo]
    segments = segments.replace(/\((.*?)\)/g, "[$1]");
    // turn all [foo=bar baz] groups into [foo="bar baz"]
    // Specifically match:    = then not " then anything till ]
    segments = segments.replace(/\=([^"][^\]]*)/g, '="$1"');
    // turn all [foo] into .foo, #foo
    // #div(adblock) means all divs with class or id adblock
    // class must be a single class, not multiple (not #*(ad listitem))
    // I haven't ever seen filters like #div(foo)(anotherfoo), so ignore these
    var resultFilter = node + segments;
    var match = resultFilter.match(/\[([^\=]*?)\]/);
    if (match)
      resultFilter = resultFilter.replace(match[0], "#" + match[1]) +
       "," + resultFilter.replace(match[0], "." + match[1]);

    return domain + "##" + resultFilter;
  },

  // Checks if the filter is an object property, which we should not overwrite.
  // See Issue 7117.
  // Throw an exeption if that's the case
  // Input: text (string): the item to check
  _checkForObjectProperty: function(text) {
    if (text in Object)
      throw "Filter causes problems in the code";
  },

  // Throw an exception if the DomainSet |domainSet| contains invalid domains.
  verifyDomains: function(domainSet) {
    for (var domain in domainSet.has) {
      if (domain === DomainSet.ALL)
        continue;
      if (/^([a-z0-9\-_\u00DF-\u00F6\u00F8-\uFFFFFF]+\.)*[a-z0-9\u00DF-\u00F6\u00F8-\uFFFFFF]+\.?$/i.test(domain) == false)
        throw Error("Invalid domain: " + domain);
      // Ensure domain doesn't break AdBlock
      FilterNormalizer._checkForObjectProperty(domain);
    }
  }
}
