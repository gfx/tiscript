function f(v: number): string {
  return v == 0
    ? `${v} is zero`
    : v > 0
      ? `${v} is positive`
      : `${v} is negative`;
}

export const r1 = f(0);
export const r2 = f(1);
export const r3 = f(-1);
