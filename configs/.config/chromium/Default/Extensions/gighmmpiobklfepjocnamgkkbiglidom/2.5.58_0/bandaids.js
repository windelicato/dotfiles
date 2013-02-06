var run_bandaids = function() {
  // Tests to determine whether a particular bandaid should be applied
  var apply_bandaid_for = "";
  if (/mail\.live\.com/.test(document.location.hostname))
    apply_bandaid_for = "hotmail";
  else if (/youtube/.test(document.location.hostname))
    apply_bandaid_for = "youtube_safari_only";
  else {
    var hosts = [ /mastertoons\.com$/ ];
    hosts = hosts.filter(function(host) { return host.test(document.location.hostname); });
    if (hosts.length > 0)
      apply_bandaid_for = "noblock";
  }

  var bandaids = {
    noblock: function() {
      var styles = document.querySelectorAll("style");
      var re = /#(\w+)\s*~\s*\*\s*{[^}]*display\s*:\s*none/;
      for (var i = 0; i < styles.length; i++) {
        var id = styles[i].innerText.match(re);
        if(id) {
          styles[i].innerText = '#' + id[1] + ' { display: none }';
        }
      }
    },
    hotmail: function() {
      //removing the space remaining in Hotmail/WLMail
      el = document.querySelector(".Unmanaged .WithSkyscraper #MainContent");
      if (el) {el.style.setProperty("margin-right", "1px", null);}
      el = document.querySelector(".Managed .WithSkyscraper #MainContent");
      if (el) {el.style.setProperty("right", "1px", null);}
      el = document.getElementById("SkyscraperContent");
      if (el) {
        el.style.setProperty("display", "none", null); 
        el.style.setProperty("position", "absolute", null); 
        el.style.setProperty("right", "0px", null); 
      }
    },

    youtube_safari_only: function() {
      function blockYoutubeAds(videoplayer) {
        var flashVars = videoplayer.getAttribute('flashvars');
        var inParam = false;
        if(!flashVars) {
            flashVars = videoplayer.querySelector('param[name="flashvars"]');
            // Give up if we still can't find it
            if(!flashVars)
                return;
            inParam = true;
            flashVars = flashVars.getAttribute("value");
        }
        var adRegex = /(^|\&)((ad_.+?|prerolls|interstitial)\=.+?|invideo\=true)(\&|$)/gi;
        if(!adRegex.test(flashVars))
            return;

        log("Removing YouTube ads");
        var adReplaceRegex = /\&((ad_\w+?|prerolls|interstitial|watermark|infringe)\=[^\&]*)+/gi;
        flashVars = flashVars.replace(adReplaceRegex, '');
        flashVars = flashVars.replace(/\&invideo\=True/i, '&invideo=False');
        flashVars = flashVars.replace(/\&ad3_module\=[^\&]*/i, '&ad3_module=about:blank');
        var replacement = videoplayer.cloneNode(true);
        if (inParam) {
            // Grab new <param> and set its flashvars
            newParam = replacement.querySelector('param[name="flashvars"]');
            newParam.setAttribute("value", flashVars);
        } else {
            replacement.setAttribute("flashvars", flashVars);
        }
        videoplayer.parentNode.replaceChild(replacement, videoplayer);
      }
      
      if (document.querySelector("#movie_player")) {
        //the movie player is already inserted
        blockYoutubeAds(document.querySelector("#movie_player"));
      } else {
        //otherwise it has to be inserted yet
        document.addEventListener("DOMNodeInserted", function(e) {
          if (e.target.id != "movie_player")
            return;
          blockYoutubeAds(e.target);
          this.removeEventListener('DOMNodeInserted', arguments.callee, false);
        }, false);
      }
    }
  }; // end bandaids

  if (apply_bandaid_for) {
    log("Running bandaid for " + apply_bandaid_for);
    bandaids[apply_bandaid_for]();
  }
}
