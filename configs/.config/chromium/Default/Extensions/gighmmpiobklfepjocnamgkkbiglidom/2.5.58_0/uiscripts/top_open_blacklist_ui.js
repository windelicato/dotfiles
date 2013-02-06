// Global lock so we can't open more than once on a tab.
if (typeof may_open_dialog_ui === "undefined")
  may_open_dialog_ui = true;

function top_open_blacklist_ui(options) {
  if (!may_open_dialog_ui)
    return;

  may_open_dialog_ui = false;

  // Get Flash objects out of the way of our UI
  BGcall('emit_page_broadcast', {fn:'send_content_to_back', options:{}});
  
  load_jquery_ui(function() {
    // If they chose "Block an ad on this page..." ask them to click the ad
    if (options.nothing_clicked)
      rightclicked_item = null;

    // If they right clicked in a frame in Chrome, use the frame instead
    if (options.info && options.info.frameUrl) {
      var frame = $("iframe").filter(function(i, el) {
        return el.src == options.info.frameUrl;
      });
      if (frame.length == 1)
        rightclicked_item = frame[0];
    }
    if (rightclicked_item && rightclicked_item.nodeName == "BODY")
      rightclicked_item = null;
    BGcall("get_settings", function(settings) {
      var advanced_user = settings.show_advanced_options;
      var blacklist_ui = new BlacklistUi(rightclicked_item, advanced_user);
      blacklist_ui.cancel(function() {
        may_open_dialog_ui = true;
      });
      blacklist_ui.block(function() {
        may_open_dialog_ui = true;
        // In case of frames, reload, as the frame might contain matches too.
        if ($("iframe, frameset, frame").filter(":visible").length > 0)
          document.location.reload();
      });
      blacklist_ui.show();
    });

  });
}

//@ sourceURL=/uiscripts/top_open_blacklist_ui.js
