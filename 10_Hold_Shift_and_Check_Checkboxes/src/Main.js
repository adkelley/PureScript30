"use strict";

exports.eqEventTarget = function (et1) {
  return function (et2) {
    return et1 === et2;
  };
};
