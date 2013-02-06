function init() {
    // Need to do some cleanup to convert to chrome.storage
    chrome.storage.local.get([ "usingStorageApi", "url" ], function(items) {
        var options = items;
        if(!options["usingStorageApi"]){
            var arr = window.localStorage.options;
            if (arr) {
                options = JSON.parse(arr);
            }
            options["usingStorageApi"] = true;
        }

        if (!options.url) {
            options.url = chrome.extension.getURL("options.html");
        }
        chrome.storage.local.set(options);
    });
}

// When installed, show welcome page
chrome.runtime.onInstalled.addListener(function() {
    chrome.storage.local.get("showWelcome", function(items) {
        if( (''+items["showWelcome"]) != "false") {
            chrome.tabs.create({"url": "welcome.html" });
        } else {
            console.log("background.js: options have suppressed welcome page");
        }
    });
});

chrome.storage.onChanged.addListener(function(changes, namespace) {
    chrome.storage.local.get("syncOptions", function(items) {
        for (key in changes) {
            var change = changes[key];
            console.log('background.js: "%s|%s" changed. "%s" -> "%s"',
                namespace,
                key,
                change.oldValue,
                change.newValue);
            
            if(items.syncOptions && namespace === "sync") {
                console.log('background.js: updating synced value %s=%s', key, change.newValue);
                var save = {};
                save[key] = change.newValue;
                chrome.storage.local.set(save);
            }
        }
    });
});

init();
