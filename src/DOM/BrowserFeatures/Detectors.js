// module DOM.BrowserFeatures.Detectors

var _inputTypeSupportMemoTable = {};

exports._detectInputTypeSupport = function(type) {
  return function() {
    if (_inputTypeSupportMemoTable.hasOwnProperty(type)) {
      return _inputTypeSupportMemoTable[type];
    }

    var el = document.createElement("input");

    try {
      el.setAttribute("type", type);
    } catch (exn) {
      return false;
    }

    var result = el.type === type;
    _inputTypeSupportMemoTable[type] = result;

    return result;
  };
};
