function add(a: number, b: number): number {
  return a + b;
}

function sub(a: number, b: number) /* return type is optional */ {
  return a - b;
}

export const a = add(1, 2);
export const b = sub(3, 4);
