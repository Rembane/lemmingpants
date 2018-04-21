"use strict";
exports._atob = function(Left, Right, s) {
  try {
    return Right(window.atob(s));
  } catch(e) {
    return Left(e);
  }
}
