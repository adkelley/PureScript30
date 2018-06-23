"use strict";

exports.innerHTML = function(html) {
  return function (eventTarget) {
    return function () {
      eventTarget.innerHTML = html;
      return {};
    };
  };
};
