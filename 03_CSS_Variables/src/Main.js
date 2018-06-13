"use strict";

exports.datasetSizing = function (elt) {
  return function () {
    return elt.dataset.sizing || "";
  };
};

exports.setStyleProperty = function (name) {
  return function (value) {
    return function (suffix) {
      return function () {
        document.documentElement.style.setProperty('--' + name, value + suffix);
        return {};
      }
    };
  };
}
