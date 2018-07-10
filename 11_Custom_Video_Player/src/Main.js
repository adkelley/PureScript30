"use strict";

exports.isVideoPaused = function(video) {
  return video.paused;
};


exports.playVideo = function(video) {
  return function() {
    video.play();
    return {};
  };
};


exports.pauseVideo = function(video) {
  return function() {
    video.pause();
    return {};
  };
};


exports.setTextContent = function(element) {
  return function(textContent) {
    element.textContent = textContent;
    return {};
  };
};
