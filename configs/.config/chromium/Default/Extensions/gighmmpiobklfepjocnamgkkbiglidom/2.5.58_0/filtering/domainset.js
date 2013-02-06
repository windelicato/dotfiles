// DomainSet: a subset of all domains.
//
// It can represent any subset of all domains.  Some examples:
//  - all domains
//  - all domains except foo
//  - only sub.foo
//  - only a, b, and c, excluding sub.a or sub.b (but including sub.sub.b)

// Create a new DomainSet from the given |data|.
//
// Each key in |data| is a subdomain, domain, or the required pseudodomain
// "DomainSet.ALL" which represents all domains.
// Each value is true/false, meaning "This domain is/is not in the set, and
// all of its subdomains not otherwise mentioned are/are not in the set."
function DomainSet(data) { 
  if (data[DomainSet.ALL] === undefined)
    throw Error("DomainSet: data[DomainSet.ALL] is undefined.");
  this.has = data; // The internal representation of our set of domains.
}

// The pseudodomain representing all domains.
DomainSet.ALL = '';

// Return the parent domain of |domain|, or DomainSet.ALL.
DomainSet._parentDomainOf = function(domain) {
  return domain.replace(/^.+?(?:\.|$)/, '');
};

// Return an object whose keys are |domain| and all of its parent domains, up
// to and including the TLD.
DomainSet.domainAndParents = function(domain) {
  var result = {};
  var parts = domain.split('.');
  var nextDomain = parts[parts.length -1];
  for (var i = parts.length-1; i >=0; i--) {
    result[nextDomain] = 1;
    if (i > 0)
      nextDomain = parts[i - 1] + '.' + nextDomain;
  }
  return result;
}

DomainSet.prototype = {

  // Returns a new identical DomainSet.
  clone: function() {
    return new DomainSet(JSON.parse(JSON.stringify(this.has)));
  },

  // Returns true if this set contains all domains.
  full: function() {
    for (var k in this.has) {
      if (!this.has[k])
        return false;
    }
    return true;
  },

  // Modify |this| by set-subtracting |other|.
  // |this| will contain the subset that was in |this| but not in |other|.
  subtract: function(other) {
    var subtract_operator = function(a,b) { return a && !b };
    this._apply(subtract_operator, other);
  },

  // NB: If we needed them, intersect and union are just like subtract, but use
  // a&&b and a||b respectively.  Union could be used to add two DomainSets.

  // Modify |this| to be the result of applying the given set |operator| (a
  // 2-param boolean function) to |this| and |other|. Returns undefined.
  _apply: function(operator, other) {
    var d; // represents a domain -- an element in .has

    // Make sure there's an entry in .has for every entry in other.has, so
    // that we examine every pairing in the next for loop.
    for (d in other.has)
      this.has[d] = this._computedHas(d);
    // Apply the set operation to each pair of entries.  Use
    // other._computedHas() to derive any missing other.has entries.
    for (d in this.has)
      this.has[d] = operator(this.has[d], other._computedHas(d));
    // Optimization: get rid of redundant entries that now exist in this.has.
    // E.g. if DomainSet.ALL, a, and sub.a all = true, delete the last 2.
    var newHas = {};
    newHas[DomainSet.ALL] = this.has[DomainSet.ALL];
    for (d in this.has)
      if (this.has[d] !== this._computedHas(DomainSet._parentDomainOf(d)))
        newHas[d] = this.has[d];
    this.has = newHas;
  },
  
  // True if |domain| is in the subset of all domains represented by |this|.
  //
  // E.g. if |this| DomainSet is the set of all domains other than a, then 'b'
  // will yield true, and both 'a' and 'sub.a' will yield false.
  _computedHas: function(domain) {
    if (this.has[domain] !== undefined)
      return this.has[domain];
    else
      return this._computedHas(DomainSet._parentDomainOf(domain));
  },

};
