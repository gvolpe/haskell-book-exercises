piProduct :: Float -> Float
piProduct x = 3.14 * x

--Rewrite the following let expressions into declarations with where clauses:
-- let x = 3; y = 1000 in x * 3 + y
a         = x * 3 + y
  where x = 3
        y = 1000

-- let y = 10; x = 10 * 5 + y in x * 5
b         = x * 5
  where y = 10
        x = 10 * 5 + y

-- let x = 7; y = negate x; z = y * 10 in z / x + y
c = z / x + y
  where x = 7
        y = negate x
        z = y * 10
