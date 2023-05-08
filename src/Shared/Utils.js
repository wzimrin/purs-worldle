"use strict";

export const debug = (a) => (b) => {
  console.log(a);
  return b;
};
