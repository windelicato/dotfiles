String.prototype.startsWith = function(str){
    return (this.indexOf(str) === 0);
}

var __down, __up;

var allowedProtocols = ["http://", "https://", "about:", "file://", "file:\\", "file:///", "chrome://", "chrome-internal://", "chrome-extension://"];

var chromePages = {
    Extensions: "chrome://extensions/",
    History: "chrome://history/",
    Downloads: "chrome://downloads/",
    NewTab: "chrome-internal://newtab/",
    Bookmarks: "chrome://bookmarks/",
    Internals: "chrome://net-internals"
}
var aboutPages = ["about:blank", "about:version", "about:plugins", "about:cache", "about:memory", "about:histograms", "about:dns", "about:terms", "about:credits", "about:net-internals"];

var popularPages = {
    "Google+": "plus.google.com",
    "Facebook": "www.facebook.com",
    "Twitter": "www.twitter.com",
    "Yahoo": "www.yahoo.com",
    "Wikipedia" : "www.wikipedia.org",
    "Digg": "www.digg.com",
    "Delicious": "www.delicious.com",
    "Slashdot": "www.slashdot.org"
};

var empty = [[]];
var langMap = {
    "options_heading": [chrome.i18n.getMessage("extName")],
    "options_subheading": empty,
    "options_status": empty,
    "options_url_label": empty,
    "options_hide_hint": empty,
    "options_localFiles_hint": empty,
    "options_quickSave_headingLarge": empty,
    "options_quickSave_headingSmall": empty,
    "options_chromePages": empty,
    "options_aboutPages": empty,
    "options_popularPages": empty,
    "options_donate_headingLarge": empty,
    "options_donate_headingSmall": empty,
    "options_anyQuestions": [
        '<a href="https://github.com/jimschubert/newtab-redirect/wiki" target="_blank">wiki</a>'
    ],
    "options_createdBy": empty,
    "options_footerPlea": empty,
    "options_githubTitle": empty,
    "btnSave": empty,
    "btnCancel": empty,
    "options_chkSync": empty,
    "options_chkShowWelcome": empty
};

// save options to localStorage.
function save_options(){
    var _url = document.getElementById('custom-url');
    var url = _url.value;
    if (url == "") {
        url = aboutPages[0];
    }

   save(url);
}

function save(url){
    clearTimeout(__up);
    clearTimeout(__down);

    var _sts = document.getElementById('status');
    var validatedUrl = getGoodUrl(url);
    var saveOptions = {"url": validatedUrl };
    saveOptions.showWelcome = document.getElementById('chkShowWelcome').checked;
    saveOptions.syncOptions = document.getElementById('chkSync').checked;
    chrome.storage.local.set(saveOptions, function() {
	_sts.innerText = chrome.i18n.getMessage("options_status",[[]]);
	_sts.style.display = "block";
	_sts.className = "slideDown";
	__up = setTimeout(function(){
		clearTimeout(__down);
		_sts.className = "slideUp";
		__down = setTimeout(function(){
			_sts.className = "";
			_sts.style.display = "none";
		}, 2000);
	}, 3050);
    });
    
    if(saveOptions.syncOptions) {
	chrome.storage.sync.set(saveOptions);
    }
}

function restore_options(){
    var savedOptions = [ "syncOptions", "showWelcome", "url" ];
    chrome.storage.local.get(savedOptions, function(items) {
	document.getElementById('custom-url').value = items.url;
	document.getElementById('chkShowWelcome').checked = 
	    items.showWelcome != undefined ? items.showWelcome : true;
	document.getElementById('chkSync').checked = 
	    items.syncOptions != undefined ? items.syncOptions : false;
    });
}

function saveQuickLink(url){
    var uurl = unescape(url);
    document.getElementById('custom-url').value = uurl;
    save(uurl);
    return false;
}

function getGoodUrl(url) {
    var goodUrl;
    
    if (protocolPasses(url) && url.length > 8) {
	console.log("getGoodUrl: Protocol accepted");
        goodUrl = url;
    } else {
        var protocol = 'http://';
        var parts = url.split('://')
        if (parts != undefined && parts != null && parts.length > 1) {
            goodUrl = protocol + parts[1];	    
	    console.log("getGoodUrl: Protocol %s not recognized, using %s", parts[0], goodUrl);
        } else {
            goodUrl = protocol + url;
	    console.log("getGoodUrl: Unexpected input. Was %s, using %s", url, goodUrl);
        }
    }
    
    return goodUrl;
}

function protocolPasses(url) {
    if (typeof(url) == 'undefined' || url == null) {
	return false;
    }
    if (url.startsWith(allowedProtocols[3]) && !url.startsWith(allowedProtocols[5])) {
	url.replace(allowedProtocols[3], allowedProtocols[5]);
    } else if (url.startsWith(allowedProtocols[4])) {
	url.replace(allowedProtocols[4], allowedProtocols[5]);
    }
    for (var p in allowedProtocols) {
	if (url.startsWith(allowedProtocols[p])) {
	    return true;
	}
    }
    return false;
}

function init(){
    document.getElementById('original-newtab')
        .addEventListener("click", function() {
            chrome.tabs.create({ "url": "chrome-internal://newtab/"});
    }, true);

    document.getElementById('btnSave')
        .addEventListener("click", save_options, true);

    document.getElementById('btnCancel')
        .addEventListener("click", restore_options, true);

    restore_options();
    var _chromes = document.getElementById('chromes');
    var _abouts = document.getElementById('abouts');
    var _pops = document.getElementById('popular');

    Object.keys(chromePages).forEach(function(key,idx) {
	var value = chromePages[key];
	var anchor = "<a data-target='" + value + "'>" + key + "</a>";
	var item = document.createElement('li');
	item.innerHTML = anchor;
	_chromes.appendChild(item);
    });
    
    for (var i = aboutPages.length - 1; i >= 0; i--){
	var $this = aboutPages[i];
	var anchor = "<a data-target='" + $this + "'>" + $this + "</a>";
	var item = document.createElement('li');
	item.innerHTML = anchor;
	_abouts.appendChild(item);
    };
    
    Object.keys(popularPages).forEach(function(key,idx) {
	var value = popularPages[key];
	var anchor = "<a data-target='" + value + "'>" + key + "</a>";
	var item = document.createElement('li');
	item.innerHTML = anchor;
	_pops.appendChild(item);
    });

    Object.keys(langMap).forEach(function(key,idx) {
        var item = key,
            extras = langMap[key];
        local(item, extras);
    });

    document.body.addEventListener("click", function(e) {
        var target = e.target && e.target.getAttribute("data-target");
        if(target) {
            saveQuickLink(target);
        }
    }, true);
}

function local(elem, supp) {
    var item = document.getElementById(elem);
    if(item) {
        var txt = chrome.i18n.getMessage(elem, supp);
        // write localized text, otherwise don't update existing text.
        if(txt) {
            item.innerHTML = chrome.i18n.getMessage(elem, supp);
        } else { console.log("chrome.i18n.getMessage: " + elem + " missing"); }
    }
}

window.addEventListener("DOMContentLoaded", init, true);

