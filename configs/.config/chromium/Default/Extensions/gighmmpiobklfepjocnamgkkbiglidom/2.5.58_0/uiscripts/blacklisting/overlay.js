Overlay = function(options) {

  var el = $(options.dom_element);

  this.image = $("<div class='adblock-killme-overlay'></div>").
    css({
      "left": el.position().left,
      "top": el.position().top
    }).
    width(el.width()).
    height(el.height());
  this.el = el;
  this.click_handler = options.click_handler;
  
  this.image.
    bind("mouseenter",function() {
      // crbug.com/110084
      this.style.setProperty("background-color", "rgba(130, 180, 230, 0.5)", "important");
    }).
    bind("mouseleave",function() {
      // crbug.com/110084
      this.style.setProperty("background-color", "transparent", "important");
    })

  Overlay.instances.push(this);
}
Overlay.instances = [];
Overlay.removeAll = function() {
  $.each(Overlay.instances, function(i,overlay) {
    overlay.image.remove();
  });
  Overlay.instances = [];
}
Overlay.prototype.display = function() {
  var that = this;
  this.image.
    appendTo(that.el.parent()).
    click(function() {
      that.click_handler(that.el);
      return false;
    });
}

//@ sourceURL=/uiscripts/blacklisting/overlay.js
