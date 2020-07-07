import Base: divgcd, numerator, denominator, string, show, ==, hastypemax,
             iszero, isone, <, <=, cmp, widen, promote_rule, isinteger, BigFloat,
             +, -, *, /, //, ^, inv, div, fld, cld, trunc, floor, ceil, round, rem, mod,
             read, write, sign, signbit, typemax, typemin, copysign, flipsign,
             abs, fma, gcd, gcdx, lcm, decompose, power_by_squaring, big

import Base.GMP: ClongMax, CulongMax, CdoubleMax


const ClongSmall = Clong === Int64 ? Union{Int8,Int16,Int32} : Union{Int8,Int16}
const CulongSmall = Culong === UInt64 ? Union{UInt8,UInt16,UInt32} : Union{UInt8,UInt16}

## Construction

BigRational(x::BigRational) = x

BigRational(x::Clong) = MPQ.set_si(x, one(Culong))
BigRational(x::ClongSmall) = BigRational(Clong(x))
BigRational(x::Culong) = MPQ.set_ui(x, one(Culong))
BigRational(x::CulongSmall) = BigRational(Culong(x))
BigRational(x::BigInt) = MPQ.set_z(x)
BigRational(x::Bool) = MPQ.set_si(Int(x), one(Culong))
BigRational(x::Integer) = BigRational(BigInt(x))

BigRational(x::Rational) = MPQ.set_den!(BigRational(numerator(x)), denominator(x))

function BigRational(x::Clong, y::Culong)
    num, den = divgcd(x, y)
    MPQ.set_si(num, den)
end
BigRational(x::ClongMax, y::CulongMax) = BigRational(Clong(x), Culong(y))
function BigRational(x::ClongSmall, y::ClongMax)
    num = flipsign(Clong(x), y)
    den = unsigned(abs(y))
    BigRational(num, den)
end
function BigRational(x::Clong, y::ClongSmall)
    BigRational(unsigned(abs(x)), flipsign(Clong(y),x))
end
function BigRational(x::Clong, y::Clong)
    if y < 0
        if x < 0
            BigRational(unsigned(-x), unsigned(-y))
        else
            BigRational(-x, unsigned(-y))
        end
    else
        BigRational(x, unsigned(y))
    end
end

function BigRational(x::Culong, y::Culong)
    num, den = divgcd(x, y)
    MPQ.set_ui(num, den)
end
BigRational(x::CulongMax, y::CulongMax) = BigRational(Culong(x), Culong(y))
function BigRational(x::CulongSmall, y::ClongMax)
    y < 0 && return BigRational(-(x%Clong), unsigned(-y))
    BigRational(x, unsigned(y))
end
function BigRational(x::Culong, y::ClongMax)
    if y < 0
        if x <= typemax(Int)
            BigRational(-(x%Clong), unsigned(-y))
        else
            MPQ.inv!(BigRational(y, x))
        end
    else
        BigRational(x, unsigned(y))
    end
end

function BigRational(x::Integer, y::Bool)
    y ? BigRational(x) : MPQ.set_si(Clong(sign(x)), Base.Checked.checked_div(y, x)%Culong)
end
function BigRational(x::Bool, y::Integer)
    if !(x && iszero(y))
        if x
            MPQ.inv!(BigRational(y))
        else
            MPQ.set_si(0, 1)
        end
    else
        MPQ.set_si(1, 0)
    end
end
function BigRational(x::Bool, y::Bool)
    y ? BigRational(x) : MPQ.set_si(1, Base.Checked.checked_div(y, x)%UInt)
end

function BigRational(x::Integer, y::Integer)
    iszero(y) && return MPQ.set_si(Clong(sign(x)), 0)
    MPQ.canonicalize!(MPQ.set_den!(BigRational(x), BigInt(y)))
end

function BigRational(x::Cdouble)
    isnan(x) && throw(InexactError(:BigRational, BigRational, x))
    isinf(x) && return MPQ.set_si(Clong(sign(x)), 0)
    MPQ.set_d(x)
end
BigRational(x::CdoubleMax) = BigRational(Cdouble(x))
function BigRational(x::AbstractFloat)
    BigRational(rationalize(BigInt, BigFloat(x)))
end


## Rational functions

numerator(x::BigRational) = MPQ.get_num(x)
denominator(x::BigRational) = MPQ.get_den(x)
# numerator(x::BigRational) = unsafe_load(Ptr{BigInt}(pointer_from_objref(x)), 1)
# denominator(x::BigRational) = unsafe_load(Ptr{BigInt}(pointer_from_objref(x)), 2)

Rational{BigInt}(x::BigRational) = Base.unsafe_rational(BigInt, numerator(x), denominator(x))
Rational(x::BigRational) = Rational{BigInt}(x)

function Rational{T}(x::BigRational) where {T <: Integer}
    Base.unsafe_rational(convert(T, numerator(x)), convert(T, denominator(x)))
end

isinteger(x::BigRational) = isone(denominator(x))

## Conversions and promotions

function BigFloat(x::BigRational, r::Base.MPFR.MPFRRoundingMode=Base.MPFR.ROUNDING_MODE[]; precision::Integer=Base.MPFR.DEFAULT_PRECISION[])
    Base.MPFR.setprecision(BigFloat, precision) do
        Base.MPFR.setrounding_raw(BigFloat, r) do
            BigFloat(numerator(x)) / BigFloat(denominator(x))
        end
    end
end
function (::Type{T})(x::BigRational) where T<:AbstractFloat
    convert(T, convert(T, numerator(x)) / convert(T, denominator(x)))
end

Bool(x::BigRational) = iszero(x) ? false : isone(x) ? true :
                       throw(InexactError(:Bool, Bool, x)) # to resolve ambiguity
function (::Type{T})(x::BigRational) where {T<:Integer}
    isinteger(x) ? convert(T, numerator(x)) : throw(InexactError(nameof(T), T, x))
end

promote_rule(::Type{BigRational}, ::Union{Type{<:Integer},Type{<:Rational}}) = BigRational
promote_rule(::Type{BigRational}, ::Type{<:AbstractFloat}) = BigFloat


## Comparisons

for (op1, op2) in ((:(==), :iszero), (:cmp, :sign))
    @eval begin
        function $op1(x::BigRational, y::Rational{<:CulongMax})
            $op2(MPQ.cmp_ui(x, numerator(y), denominator(y)))
        end
        function $op1(x::BigRational, y::Rational{<:ClongMax})
            $op2(MPQ.cmp_si(x, numerator(y), denominator(y)))
        end
        $op1(x::BigRational, y::ClongMax) = $op2(MPQ.cmp_si(x, Clong(y), one(Culong)))
        $op1(x::BigRational, y::CulongMax) = $op2(MPQ.cmp_ui(x, Culong(y), one(Culong)))
        $op1(x::BigRational, y::BigInt) = $op2(MPQ.cmp_z(x, y))
        $op1(x::BigRational, y::Bool) = $op1(x, Clong(y))
        $op1(x::BigRational, y::Integer) = $op1(x, BigInt(y))
    end
end

==(x::BigRational, y::BigRational) = MPQ.equal(x, y)
function ==(x::BigRational, y::Rational)
    numerator(x) == numerator(y) && denominator(x) == denominator(y)
end
==(x::Union{Rational,Integer}, y::BigRational) = y == x
cmp(x::BigRational, y::BigRational) = sign(MPQ.cmp(x, y))
cmp(x::BigRational, y::Rational{BigInt}) = cmp(x, BigRational(y))
function cmp(x::BigRational, y::Rational)
    cmp(denominator(y) * numerator(x), denominator(x) * numerator(y))
end

cmp(x::BigRational, y::Real) = cmp(numerator(x), y*denominator(x))
cmp(y::Union{Rational,Real}, x::BigRational) = -cmp(x, y)

iszero(x::BigRational) = iszero(x.num_size)
isone(x::BigRational) = isone(numerator(x)) && isone(denominator(x))

for op in (:(<), :(<=))
    for (T1, T2) in ((:BigRational, :Real), (:Real, :BigRational), (:BigRational, :BigRational))
        @eval $op(x::$T1, y::$T2) = isnan(x) ? false : isnan(y) ? false : ($op)(cmp(x,y), 0)
    end
end


## Arithmetic operations

-(x::BigRational) = MPQ.neg(x)

for (op, gop) in ((:+, :add), (:-, :sub), (:*, :mul), (:/, :div), (://, :div))
    gop! = Symbol(gop, :!)
    @eval begin
        $op(x::BigRational, y::BigRational) = MPQ.$gop(x, y)
        function $op(x::BigRational, y::Union{Integer,Rational})
            z = BigRational(y) # z !== y by construction because !(y isa BigRational)
            MPQ.$gop!(z, x, z)
        end
        function $op(x::Union{Integer,Rational}, y::BigRational)
            z = BigRational(x) # z !== x by construction because !(x isa BigRational)
            MPQ.$gop!(z, z, y)
        end
    end
end

^(x::Number, y::BigRational) = x^(float(y))
^(::Irrational{:ℯ}, x::BigRational) = exp(x) # to avoid ambiguity
^(x::T, y::BigRational) where {T<:AbstractFloat} = x^convert(T,y)
^(z::Complex{T}, p::BigRational) where {T<:Real} = z^convert(typeof(one(T)^p), p)
^(z::Complex{BigRational}, n::Bool) = n ? z : one(z) # to resolve ambiguity
function ^(z::Complex{BigRational}, n::Integer)
    n >= 0 ? Base.power_by_squaring(z, n) : power_by_squaring(inv(z), -n)
end
# All previous functions on ^ are taken from base/rational.jl

function power_by_squaring(x::BigRational, p::Integer)
    if p <= 2
        if p == 1
            return x
        elseif p == 0
            return one(BigRational)
        elseif p == 2
            return x*x
        elseif p == -1
            return inv(x)
        else
            power_by_squaring(inv(x), -p)
        end
    end
    t = trailing_zeros(p) + 1
    p >>= t
    z = MPQ.set(x)
    while (t -= 1) > 0
        MPQ.mul!(z, z)
    end
    y = MPQ.set(z)
    while p > 0
        t = trailing_zeros(p) + 1
        p >>= t
        while (t -= 1) >= 0
            MPQ.mul!(z, z)
        end
        MPQ.mul!(y, z)
    end
    return y
end

inv(x::BigRational) = MPQ.inv(x)

# The next few functions might need to be optimized
for op in (:rem, :mod)
    @eval begin
        function $op(x::BigRational, y::Integer)
            den = denominator(x)
            BigRational($op(numerator(x), den * y), den)
        end
        function $op(x::Integer, y::BigRational)
            den = denominator(y)
            BigRational($op(x * den, numerator(y)), den)
        end
    end
    for (T1, T2) in ((:Rational, :BigRational), (:BigRational, :Rational), (:BigRational, :BigRational))
        @eval begin
            function $op(x::$T1, y::$T2)
                den = denominator(x)
                xd, yd = divgcd(den, denominator(y))
                BigRational($op(numerator(x)*yd, numerator(y)*xd), den*yd)
            end
        end
    end
end
function div(x::BigRational, y::Integer, r::RoundingMode)
    xn, yn = divgcd(numerator(x), y)
    div(xn, denominator(x) * yn, r)
end
function div(x::Integer, y::BigRational, r::RoundingMode)
    xn, yn = divgcd(x, numerator(y))
    div(xn * denominator(y), yn, r)
end
for (T1, T2) in ((:Rational, :BigRational), (:BigRational, :Rational), (:BigRational, :BigRational))
    @eval begin
        function div(x::$T1, y::$T2, r::RoundingMode)
            xn, yn = divgcd(numerator(x), numerator(y))
            xd, yd = divgcd(denominator(x), denominator(y))
            div(xn * yd, xd * yn, r)
        end
    end
end

for (S, T) in ((BigRational, Integer), (Integer, BigRational), (BigRational, BigRational))
    @eval begin
        div(x::$S, y::$T) = div(x, y, RoundToZero)
        fld(x::$S, y::$T) = div(x, y, RoundDown)
        cld(x::$S, y::$T) = div(x, y, RoundUp)
    end
end

trunc(::Type{T}, x::BigRational) where {T} = round(T, x, RoundToZero)
floor(::Type{T}, x::BigRational) where {T} = round(T, x, RoundDown)
ceil(::Type{T}, x::BigRational) where {T} = round(T, x, RoundUp)
for f in (:ceil, :floor, :trunc, :round)
    @eval begin
        $f(::Type{T}, x::BigRational) where {T>:Missing} = $f(Base.nonmissingtype_checked(T), x)
    end # to avoid ambiguity
end

function round(::Type{T}, x::BigRational, r::RoundingMode=RoundNearest) where {T>:Missing}
    round(Base.nonmissingtype_checked(T), x, r)
end # to avoid ambiguity
round(x::BigRational, r::RoundingMode=RoundNearest) = round(typeof(x), x, r)

function round(::Type{T}, x::BigRational, r::RoundingMode=RoundNearest) where T
    if iszero(denominator(x)) && !(T <: Integer)
        return convert(T, Rational{Int32}(x))
    end
    convert(T, div(numerator(x), denominator(x), r))
end


function gcd(x::BigRational, y::BigRational)
    BigRational(gcd(numerator(x), numerator(y)), lcm(denominator(x), denominator(y)))
end
function lcm(x::BigRational, y::BigRational)
    BigRational(lcm(numerator(x), numerator(y)), gcd(denominator(x), denominator(y)))
end
function gcdx(x::BigRational, y::BigRational)
    c = gcd(x, y)
    num = numerator(c)
    den = denominator(c)
    if iszero(num)
        a, b = one(BigInt), zero(BigInt)
    elseif iszero(den)
        a = ifelse(iszero(denominator(x)), one(BigInt), den)
        b = ifelse(iszero(denominator(y)), one(BigInt), den)
    else
        _, a, b = gcdx(div(numerator(x), num) * div(den, denominator(x)),
                       div(numerator(y), num) * div(den, denominator(y)))
    end
    c, a, b
end


sign(x::BigRational) = sign(numerator(x))
signbit(x::BigRational) = signbit(numerator(x))

copysign(x::BigRational, y::Real) = signbit(x) == signbit(y) ? x : -x

flipsign(x::BigRational, y::Real) = signbit(y) ? -x : x

abs(x::BigRational) = MPQ.abs(x)


## IO operations

function string(x::BigRational; base::Integer = 10, pad::Integer = 1)
    string(BigRational, '(' * string(numerator(x); base, pad)   * ',' *
                              string(denominator(x); base, pad) * ')')
end
function show(io::IO, x::BigRational)
    if get(io, :typeinfo, Any) == BigRational
        print(io, numerator(x), "//", denominator(x))
    else
        print(io, string(x))
    end
end

function read(io::IO, ::Type{BigRational})
    num = read(s, BigInt)
    den = read(s, BigInt)
    BigRational(num, den)
end
function write(io::IO, x::BigRational)
    write(io, numerator(x), denominator(x))
end


## Other functions

widen(::Type{BigRational}) = BigRational

big(::Type{BigRational}) = BigRational
big(x::BigRational) = x

hastypemax(::Type{BigRational}) = true
typemax(::Type{BigRational}) = BigRational(1,0)
typemin(::Type{BigRational}) = BigRational(-1,0)

decompose(x::BigRational) = numerator(x), 0, denominator(x)

function fma(x::BigRational, y::BigRational, z::BigRational)
    MPQ.add!(MPQ.mul(x, y), z)
end

function Base.deepcopy_internal(x::BigRational, stackdict::IdDict)
    if haskey(stackdict, x)
        return stackdict[x]
    end
    y = MPQ.set(x)
    stackdict[x] = y
    return y
end
