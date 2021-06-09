"use strict";

var readlineSync = require('readline-sync')

exports.readLine = function (prompt) {
  return readlineSync.question(prompt)
}