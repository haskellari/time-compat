# time-compat

## Updating for new `time`

- Copy test directory from `time` to `time-compat`
- Then update the imports and reformat with `ormolu`

```
sed -i 's/import Data.Time.*/\0.Compat/g' test/**/*.hs
ormolu -i test/**/*.hs
```

- Check changelog and `cabal-diff` e.g.

```
cabal-diff time 1.14 1.15 -w ghc-9.10.2
```

for changes to backport.
