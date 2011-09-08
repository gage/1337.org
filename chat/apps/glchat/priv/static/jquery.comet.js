(function($) {

    $.comet = function(key, callback) {
        var timestamp = (new Date()).getTime();
        $.comet.callbacks[key] = callback;
    };

    $.comet.onReadyCBs = new Array();

    $.comet.onReady = function(callback) {
      if ($.comet.id) callback();
      $.comet.onReadyCBs.push(callback);
    };

    $.comet.init = function(baseURL, credentials, readyCallback, failCallback) {
        $.comet.callbacks = {};
        $.comet.baseURL = baseURL;

        if (!failCallback) failCallback = function(jqXHR, textStatus, errorThrown) {
          if (console && console.debug) {
            console.debug("Init failed: " + jqXHR.responseText);
            console.debug(errorThrown);
          }
        };

        if (readyCallback) $.comet.onReady(readyCallback);

        $.comet.call({
          method: "start",
          data: credentials,
          success: function(data) {
            if (data['success']) {
              $.comet.id = data['uuid'];
              for (var i = 0; i < $.comet.onReadyCBs.length; i++) {
                $.comet.onReadyCBs[i](data);
              }
              $.comet.poll();
            } else {
              falCallback();
            }
          },
          error: failCallback
        });

    };

    $.comet.poll = function() {
      $.comet.call({
        method: "poll",
        success: $.comet.messages,
        // XXX BGH TODO: Retry / reload on poll failure
        error: function() { $.fn.glchat.output("Connection lost!"); }
      });
    };

    $.comet.call = function(ajaxInfo) {
      ajaxInfo['cache'] = false;
      ajaxInfo['dataType'] = "json";
      ajaxInfo['type'] = "post";
      if (!ajaxInfo['data']) ajaxInfo['data'] = {};
      ajaxInfo['data']['id'] = $.comet.id;

      if (!ajaxInfo['url'] && ajaxInfo['method']) {
        ajaxInfo['url'] = $.comet.baseURL + "/" + ajaxInfo['method'];
      }
      if (!ajaxInfo['error']) {
        ajaxInfo['error'] = $.fn.glchat.output;
      }
      $.ajax(ajaxInfo);
    };

    $.comet.messages = function(json) {
        $.each(json, function(i, obj) {
            var callback = $.comet.callbacks[obj.key];
            if (callback) {
                try { callback(obj); }
                catch (e) {
                    if (console && console.debug) {
                        console.debug("Oh noes: " + e);
                    }
                }
            }
        });
        setTimeout(function() { $.comet.poll(); }, 10);
    };

})(jQuery);