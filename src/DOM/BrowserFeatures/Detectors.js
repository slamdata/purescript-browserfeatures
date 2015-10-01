// module DOM.BrowserFeatures.Detectors

exports._getTypeProperty = function (el) {
  return function () {
    return el.type;
  };
};
