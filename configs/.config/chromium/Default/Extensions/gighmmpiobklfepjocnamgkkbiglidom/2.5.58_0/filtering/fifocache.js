// Simple size-limited FIFO cache that does not support delete.

// Create a cache that can contain up to size items.  Further items added to
// the cache will evict the oldest items in the cache.
function FifoCache(size) {
  this._size = Math.max(size, 0);
  this._cacheKeys = [];
  this._cache = {};
}
FifoCache.prototype = {
  // Add an entry to the cache, evicting the (size)th-oldest entry if the cache
  // is full.
  set: function(key, value) {
    var alreadyCached = (key in this._cache);
    this._cache[key] = value;

    if (!alreadyCached) {
      this._cacheKeys.push(key); // add to the end
      if (this._cacheKeys.length > this._size) {
        delete this._cache[this._cacheKeys[0]];
        this._cacheKeys.shift(); // remove from the beginning
      }
    }
  },

  // Return an entry from the cache, or undefined if key is not in the cache.
  get: function(key) {
    return this._cache[key];
  }
};
