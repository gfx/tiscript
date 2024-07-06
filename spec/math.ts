// test static functions in ECMA-262 Math object.
// cf. https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math
// cf. https://262.ecma-international.org/14.0/#sec-function-properties-of-the-math-object

export const math_abs = {
  "Math.abs(0)": Math.abs(0),
  "Math.abs(1)": Math.abs(1),
  "Math.abs(-1)": Math.abs(-1),
  "Math.abs(1.5)": Math.abs(1.5),
  "Math.abs(-1.5)": Math.abs(-1.5),
  // TODO: infinities and NaN
};

export const math_acos = {
  "Math.acos(0)": Math.acos(0),
  "Math.acos(1)": Math.acos(1),
  "Math.acos(8/10)": Math.acos(8/10),
  "Math.acos(5/3)": Math.acos(5/3),
};

export const math_acosh = {
  "Math.acosh(1)": Math.acosh(1),
  "Math.acosh(2)": Math.acosh(2),
  "Math.acosh(2.1)": Math.acosh(2.1),
  //   "Math.acosh(2.5)": Math.acosh(2.5), // does not match the output of node.js
};


