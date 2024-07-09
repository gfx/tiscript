// regression: as of 33edc48d1, this code causes a stack overflow error.
const a = 1;
export const b = 2;
export const c = Math.sqrt(a);
