// Schedules a function to be executed once when the computer is idle.
// Call idleHandler.scheduleItem to schedule a function for exection upon idle
// inputs: theFunction: function to be executed
//         seconds: maximum time to wait upon idle, in seconds. 600 if omitted.
var idleHandler = {
  scheduleItemOnce:
    function(callback, seconds) {
      // In  Safari, execute the function with only the minimum idle delay.
      // It doesn't support idle, but at least we make sure that functions
      // don't execute when we're too busy to handle them.
      if (SAFARI) {
        window.setTimeout(callback, 15000);
        return;
      }
      // In Chrome, schedule the item to be executed
      idleHandler._scheduledItems.push({
        callback: callback,
        runAt: new Date(Date.now() + 1000 * (seconds || 600))
      });
      if (!idleHandler._timer)
        idleHandler._timer = window.setInterval(idleHandler._runIfIdle, 5000);
    },
  _timer:
    null,
  _scheduledItems:
    [],
  _runIfIdle:
    function() {
      // Checks if the browser is idle. If so, it executes all waiting functions
      // Otherwise, it checks if an item has waited longer than allowed, and
      // executes the ones who should be executed
      chrome.idle.queryState(15, function(state) {
        if (state == "idle") {
          while (idleHandler._scheduledItems.length)
            idleHandler._scheduledItems.shift().callback();
        } else {
          var now = Date.now();
          // Inversed loop, to prevent splice() making it skip the item after an
          // executed item.
          for (var i=idleHandler._scheduledItems.length-1; i>=0; i--)
            if (idleHandler._scheduledItems[i].runAt <= now)
              idleHandler._scheduledItems.splice(i, 1)[0].callback();
        }
        if (!idleHandler._scheduledItems.length)
          idleHandler._timer = window.clearInterval(idleHandler._timer);
      })
    }
};
