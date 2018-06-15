"use strict";

exports.table = function (xs) {
  return function () {
    console.table(xs);
    return {};
  };
};
