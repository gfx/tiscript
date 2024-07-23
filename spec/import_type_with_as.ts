import type { Foo, Bar as Baz } from './types/foo.ts';
import type * as ns from './types/foo.ts';
import type One from './types/one.ts';

export const bar: Baz = {
  one: 1,
  two: 2,
};

export const foo: Foo = {
  key: 'foo',
  value: 'bar',
};

export const nsFoo: ns.Foo = {
  key: 'FOO',
  value: 'BAR',
};

export const one: One = {
  key: 'hoge',
  value: 'fuga',
};
