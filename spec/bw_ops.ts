const a = 5; //  00000000000000000000000000000101
const b = 2; //  00000000000000000000000000000010
const c = -5; //  11111111111111111111111111111011

// XXX: The order matter because of Node.js implementation details.

export const bwa1 = a & b;
export const bwa2 = a & c;
export const bwls1 = a << b;
export const bwls2 = c << b;
export const bwn1 = ~a;
export const bwn2 = ~b;
export const bwn3 = ~c;
export const bwo1 = a | b;
export const bwo2 = a | c;
export const bwrs1 = a >> b;
export const bwrs2 = c >> b;
export const bwrsu1 = a >>> b;
export const bwrsu2 = c >>> b;
export const bwx1 = a ^ b;
export const bwx2 = a ^ c;
