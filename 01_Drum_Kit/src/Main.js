"use strict";

exports.propertyName = function (event) {
  return event.propertyName;
};

exports.srcElement = function (event) {
  return event.srcElement;
}

exports.audioCurrentTime = function (time) {
  return function (audioElement) {
    return function () {
      audioElement.currentTime = time;
      return {};
    };
  };
};

exports.playAudio = function (audioElement) {
  return function () {
    audioElement.play();
    return {};
  };
};

exports.innerHTML = function(eventTarget) {
  return function (html) {
    return function () {
      eventTarget.innerHTML = html;
      return {};
    };
  };
};
