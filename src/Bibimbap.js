function GenericSumError(tag, genericSum) {
  this.genericSum = genericSum;
  this.genericSumTag = tag;
  this.message = "GenericSumError";
  return this;
}

GenericSumError.prototype = Object.create(Error.prototype);
GenericSumError.prototype.constructor = GenericSumError;
GenericSumError.prototype.name = "GenericSumError";

exports._mkGenericSumError = function(tag, genericSum) {
  return new GenericSumError(tag, genericSum);
};

exports._readGenericSumError = function(test, nothing, just, error) {
  if (error instanceof GenericSumError) {
    var genericSum = error.genericSum;
    var genericSumTag = error.genericSumTag;
    if (test(genericSumTag)) {
      return just(error);
    } else {
      return nothing;
    }
  } else {
    return nothing;
  }
};

exports._getGenericSum = function(genericSumError) {
  return genericSumError.genericSum;
};
