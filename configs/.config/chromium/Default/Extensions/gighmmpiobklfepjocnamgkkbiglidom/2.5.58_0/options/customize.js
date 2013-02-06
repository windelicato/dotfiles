chrome.extension.onRequest.addListener(function(request) {
  if (request.command != "filters_updated")
    return;
  if ($("#txtFiltersAdvanced").prop("disabled") === false)
    return;
  BGcall("get_custom_filters_text", function(text) {
    $("#txtFiltersAdvanced").val(text);
  });
});

$(function() {
  // Add a custom filter to the list
  function appendCustomFilter(filter) {
    var customFilterText = $("#txtFiltersAdvanced").val();
    $("#txtFiltersAdvanced").val(filter + "\n" + customFilterText);
    saveFilters();
    $(".addControls").slideUp();
  }

  // Convert a messy list of domains to ~domain1.com|~domain2.com format
  function toTildePipeFormat(domainList) {
    domainList = domainList.trim().replace(/[\ \,\;\|]+\~?/g, "|~");
    if (domainList && domainList[0] != "~")
      domainList = "~" + domainList;
    return domainList;
  }

  $("#txtBlacklist").focus(function() {
    // Find the blacklist entry in the user's filters, and put it
    // into the blacklist input.
    var customFilterText = $("#txtFiltersAdvanced").val();
    var match = customFilterText.match(/^\@\@\*\$document\,domain\=(\~.*)$/m);
    if (match && $(this).val() == "")
      $(this).val(match[1]);
  });

  // The add_filter functions
  $("#btnAddUserFilter").click(function() {
    var blockCss = $("#txtUserFilterCss").val().trim();
    var blockDomain = $("#txtUserFilterDomain").val().trim();

    if (blockDomain == '.*' || blockDomain == "*" || blockDomain == '')
      appendCustomFilter("##" + blockCss);
    else
      appendCustomFilter(blockDomain + "##" + blockCss);

    $(this).closest(".entryTable").find("input[type='text']").val("");
    $(this).attr("disabled", "disabled");
  });

  $("#btnAddExcludeFilter").click(function() {
    var excludeUrl = $("#txtUnblock").val().trim();

    //prevent regexes
    if (/^\/.*\/$/.test(excludeUrl))
      excludeUrl = excludeUrl + "*";

    appendCustomFilter('@@' + excludeUrl + '$document');

    $(this).closest(".entryTable").find("input[type='text']").val("");
    $(this).attr("disabled", "disabled");
  });

  $("#btnAddBlacklist").click(function() {
    var blacklist = toTildePipeFormat($("#txtBlacklist").val());

    var filters = $("#txtFiltersAdvanced").val().trim() + '\n';
    // Delete the first likely line
    filters = filters.replace(/^\@\@\*\$document,domain\=~.*\n/m, "").trim();
    $("#txtFiltersAdvanced").val(filters);
    // Add our line in its place, or if it was empty, remove the filter
    if (blacklist)
      appendCustomFilter("@@*$document,domain=" + blacklist);
    else
      saveFilters(); // just record the deletion
    $("#btnAddBlacklist").attr("disabled", "disabled");
  });

  $("#btnAddUrlBlock").click(function() {
    var blockUrl = $("#txtBlockUrl").val().trim();
    var blockDomain = $("#txtBlockUrlDomain").val().trim();
    if (blockDomain == '*')
      blockDomain = '';

    //prevent regexes
    if (/^\/.*\/$/.test(blockUrl))
      blockUrl = blockUrl + "*";

    if (blockDomain == '')
      appendCustomFilter(blockUrl);
    else
      appendCustomFilter(blockUrl + "$domain=" + blockDomain);

    $(this).closest(".entryTable").find("input[type='text']").val("");
    $(this).attr("disabled", "disabled");
  });

  // The validation functions
  $("#txtBlacklist").bind("input", function() {
    var blacklist = toTildePipeFormat($("#txtBlacklist").val());

    if (blacklist)
      blacklist = "@@*$document,domain=" + blacklist;

    try {
      FilterNormalizer.normalizeLine(blacklist);
      $("#btnAddBlacklist").removeAttr("disabled");
    } catch(ex) {
      $("#btnAddBlacklist").attr("disabled", "disabled");
    }
  });

  $("#divUrlBlock input[type='text']").bind("input", function() {
    var blockUrl = $("#txtBlockUrl").val().trim();
    var blockDomain = $("#txtBlockUrlDomain").val().trim();
    if (blockDomain == '*')
      blockDomain = '';
    if (blockDomain)
      blockDomain = '$domain=' + blockDomain;
    var ok = false;
    try {
      if (FilterNormalizer.normalizeLine(blockUrl + blockDomain))
        ok = true;
      if (Filter.isSelectorFilter(blockUrl))
        ok = false;
    } catch(ex) {}
    $("#btnAddUrlBlock").attr("disabled", ok ? null : "disabled");
  });

  $("#divCssBlock input[type='text']").bind("input", function() {
    var blockCss = $("#txtUserFilterCss").val().trim();
    var blockDomain = $("#txtUserFilterDomain").val().trim();
    if (blockDomain == '*')
      blockDomain = '';
    var ok = false;
    try {
      if (FilterNormalizer.normalizeLine(blockDomain + "##" + blockCss))
        ok = true;
    } catch(ex) {}
    $("#btnAddUserFilter").attr("disabled", ok ? null : "disabled");
  });

  $("#divExcludeBlock input[type='text']").bind("input", function() {
    var unblockUrl = $("#txtUnblock").val().trim();
    var ok = false;
    try {
      if (FilterNormalizer.normalizeLine('@@' + unblockUrl + '$document'))
        ok = true;
      if (!unblockUrl || Filter.isSelectorFilter(unblockUrl))
        ok = false;
    } catch(ex) {}
    $("#btnAddExcludeFilter").attr("disabled", ok ? null : "disabled");
  });

  // When one presses 'Enter', pretend it was a click on the 'add' button
  $(".entryTable input[type='text']").keypress(function(event) {
    var submitButton = $(this).closest(".entryTable").find("input[type='button']");
    if (event.keyCode === 13 && !submitButton.prop("disabled")) {
      event.preventDefault();
      submitButton.click();
    }
  });

  $("a.controlsLink").click(function() {
    event.preventDefault();
    var myControls = $(this).closest("div").find(".addControls");
    $(".addControls").not(myControls).slideUp();
    myControls.slideToggle();
  });

  $("#btnEditAdvancedFilters").click(function() {
    $("#divAddNewFilter").slideUp();
    $(".addControls").slideUp();
    $("#txtFiltersAdvanced").removeAttr("disabled");
    $("#spanSaveButton").show();
    $("#btnEditAdvancedFilters").hide();
    $("#txtFiltersAdvanced").focus();
  });

  function saveFilters() {
    BGcall("set_custom_filters_text", $("#txtFiltersAdvanced").val());

    $("#divAddNewFilter").slideDown();
    $("#txtFiltersAdvanced").attr("disabled", "disabled");
    $("#spanSaveButton").hide();
    $("#btnEditAdvancedFilters").show();
  }
  $("#btnSaveAdvancedFilters").click(saveFilters);

  BGcall("get_custom_filters_text", function(text) {
    $("#txtFiltersAdvanced").val(text);
  });


  $("#btnCleanUp").click(function() {
    //Don't save immediately, first allow them to review changes
    if ($("#btnEditAdvancedFilters").is(":visible"))
      $("#btnEditAdvancedFilters").click();
    var newFilters = FilterNormalizer.normalizeList($("#txtFiltersAdvanced").val(), true);
    newFilters = newFilters.replace(/(\n)+$/,'\n'); // Del trailing \n's
    $("#txtFiltersAdvanced").val(newFilters);
  });
});
