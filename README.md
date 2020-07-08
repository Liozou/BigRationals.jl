# BigRationals

[![Build Status](https://travis-ci.com/Liozou/BigRationals.jl.svg?branch=master)](https://travis-ci.com/Liozou/BigRationals.jl)
[![Build Status](https://ci.appveyor.com/api/projects/status/github/Liozou/BigRationals.jl?svg=true)](https://ci.appveyor.com/project/Liozou/BigRationals-jl)
[![codecov](https://codecov.io/gh/Liozou/BigRationals.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/Liozou/BigRationals.jl)

This package provides a wrapper around the GNU Multiple Precision (GMP) rational type `mpq_t` into the new Julia type `BigRational`. It requires GMP to be installed.
For interoperability with Julia's native `BigInt` type (which is a wrapper around GMP's integer type `mpz_t`), the same version of GMP should be used for both Julia and this package. This package is not tested for systems where the default GMP installation version is not the same as Julia's.

GMP functions for rationals are wrapped in the `MPQ` module, similar to `Base.GMP.MPZ`. For instance, to use faster in-place operations, one may do:
```julia
julia> using BigRationals

julia> x = BigRational(3,4)
BigRational(3,4)

julia> BigRationals.MPQ.add!(x, BigRational(2,3))
BigRational(17,12)

julia> x
BigRational(17,12)
```

The `BigRational` type can also be used with the full interface of the native `Rational` type, including the `numerator` and `denominator` functions, comparisons (`==`, `<=`, etc.), arithmetic (`+`, `-`, `*`, `/`, etc.) and others. It is also defined as a subtype of `Real` and can be used as such.

`BigRational` aims to be a faster replacement for `Rational{BigInt}`. However, both types coexist when using this package and `Rational{BigInt}` can still be used as before. Conversion between the two types is as simple as:
```julia
julia> x = BigRational(3,4)
BigRational(3,4)

julia> y = Rational(x)
3//4

julia> typeof(y)
Rational{BigInt}

julia> BigRational(y)
BigRational(3,4)
```

Be warned that, even with this package, `widen(Rational{Int128}) == Rational{BigInt}` and `big(Rational) == Rational{BigInt}` and not `BigRational`, since changing these behaviours would be type piracy.

