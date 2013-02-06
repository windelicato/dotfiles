function r(tabId, url) {
    chrome.tabs.update(tabId, {
	"url": url || chrome.extension.getURL("options.html"),
	"selected": true
    });
}

function init(){    
    chrome.storage.local.get("url", function(items) {
	console.log("Items:");
	console.log(items);
	var url = items.url || "";
	if ((/^http:/i.test(url)) || /^https:/i.test(url)) {
	    document.location.href = url;
	    return;
	} else {
	    chrome.tabs.getCurrent(function(t) {
		r(t.id, url);
	    });
	}
    });
}
window.addEventListener("DOMContentLoaded", init, true);
