exports.setFocusImpl = function(elemId) {
  return function() {
    document.getElementById(elemId).focus();
  };
};
