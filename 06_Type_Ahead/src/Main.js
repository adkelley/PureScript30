"use strict";

exports.innerHTML = function(eventTarget) {
  return function (html) {
    return function () {
      eventTarget.innerHTML = html;
      return {};
    };
  };
};
