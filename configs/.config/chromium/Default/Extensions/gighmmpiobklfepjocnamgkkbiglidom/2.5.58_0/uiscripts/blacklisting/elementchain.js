// Requires jquery.

// Lets you move up and down the DOM starting from a specific element.
function ElementChain(el) {
  this._stack = [];
  this._change_events = [];
  this._stack.push($(el));
}
ElementChain.prototype.current = function() {
  return this._stack[this._stack.length - 1];
}
ElementChain.prototype.moveUp = function() {
  if (this.current().parent().length > 0 &&
      this.current().parent()[0].nodeName != "BODY") {
    this._stack.push(this.current().parent());
    this.change();
    return true;
  }
  return false;
}
ElementChain.prototype.moveDown = function() {
  if (this._stack.length > 1) {
    this._stack.pop();
    this.change();
    return true;
  }
  return false;
}
// Moves to the appropriate parent depth.  0 is the original element,
// 1 is its parent, etc.
ElementChain.prototype.moveTo = function(depth) {
  while (this._stack.length > depth + 1)
    if (!this.moveDown())
      break;
  while (this._stack.length < depth + 1)
    if (!this.moveUp())
      break;
}
ElementChain.prototype.change = function(listener, callback) {
  if (callback) {
    this._change_events.push([listener, callback]);
  } else {
    for (var i = 0; i < this._change_events.length; i++) {
      var data = this._change_events[i];
      data[1].call(data[0]);
    }
  }
}

//@ sourceURL=/uiscripts/blacklisting/elementchain.js