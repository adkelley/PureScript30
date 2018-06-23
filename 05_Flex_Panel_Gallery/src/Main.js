"use strict";

exports.logIt = function (a) {
  return function () {
    console.log(a);
    return {};
  };
};

exports.propertyName = function (event) {
  return function () {
    return event.propertyName;
  };
};
