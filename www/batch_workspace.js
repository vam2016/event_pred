(function () {
  'use strict';
  var memoryToken;
  function token() {
    var value;
    try { value = window.localStorage.getItem('event_pred.batch_workspace.v1'); } catch (_) {}
    if (!/^[a-f0-9]{32}$/.test(value || '')) {
      value = memoryToken;
      if (!value) {
        var bytes = new Uint8Array(16);
        window.crypto.getRandomValues(bytes);
        value = Array.from(bytes, function (b) { return b.toString(16).padStart(2, '0'); }).join('');
        memoryToken = value;
      }
      try { window.localStorage.setItem('event_pred.batch_workspace.v1', value); } catch (_) {}
    }
    return value;
  }
  window.jQuery(document).on('shiny:connected', function () {
    window.Shiny.setInputValue('ba_workspace_token', token(), {priority: 'event'});
  });
}());
