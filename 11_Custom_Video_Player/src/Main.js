"use strict";

exports._isVideoPaused = function(video) {
  return video.paused;
};


exports.playVideo = function(video) {
  return function() {
    video.play();
  };
};


exports.pauseVideo = function(video) {
  return function() {
    video.pause();
  };
};

exports.videoCurrentTime = function(video) {
  return function() {
    return video.currentTime;
  };
};

exports.setVideoCurrentTime = function(video) {
  return function(time) {
    return function () {
      video.currentTime = time;
    };
  };
};


exports.setTextContent = function(element) {
  return function(textContent) {
    return function () {
      element.textContent = textContent;
    };
  };
};

exports.videoSkipTime = function(button) {
  return function() {
    return button.dataset.skip;
  };
};


exports.eventTargetName = function(eventTarget) {
  return function () {
    return eventTarget.name;
  };
};

exports.eventTargetValue = function(eventTarget) {
  return function () {
    return eventTarget.value;
  };
};

exports.setVideoPlaybackRate = function(video) {
  return function (rate) {
    return function () {
      video.playbackRate = rate;
      return {};
    };
  };
};


exports.setVideoVolume = function(video) {
  return function (volume) {
    return function () {
      video.volume = volume;
      return {};
    };
  };
};

exports.videoDuration = function (video) {
  return function () {
    return video.duration;
  };
};

exports.updateProgressBar = function(element) {
  return function (percent) {
    return function () {
      element.style.flexBasis = percent + "%";
    };
  };
};


exports.offsetX = function(mouseEvent) {
  return function () {
    return mouseEvent.offsetX;
  }
}


exports.offsetWidth = function (element) {
  return function () {
    return element.offsetWidth;
  }
}
