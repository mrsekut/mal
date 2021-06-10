"use strict";

var readlineSync = require('readline-sync')

exports.readLine = function () {
  return readlineSync.question('user> ')
}