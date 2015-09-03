// module DOM.BrowserFeatures.Detectors

exports._detectInputTypeSupport = function(type) {
  return function() {
    var el = document.createElement("input");

    try {
      el.setAttribute("type", type);
    } catch (exn) {
      return false;
    }

    return el.type === type;
  };
};
