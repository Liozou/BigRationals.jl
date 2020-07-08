using BigRationals
using Test
using Aqua

Aqua.test_all(BigRationals)

ONE = BigRational(1,1)
ZERO = BigRational(0,1)

@testset "Construction" begin
    @test ZERO == zero(BigRational) == 0
    @test iszero(ZERO)
    @test ONE == one(BigRational) == 1
    @test isone(ONE)
    @test BigRational(2,2) == UInt(1)
    @test BigRational(1,-3) == BigRational(-1,3)
    @test BigRational(-4,-6) == BigRational(2//3) == 2//3

    @test BigRational(-4) == BigRational(Int8(-4)) == BigRational(Int128(-4))
    @test BigRational(UInt(6)) == BigRational(BigInt(6)) == BigRational(6)
    @test BigRational(UInt16(1))  == BigRational(true) == BigRational(ONE) == 0x1

    @test BigRational(typemin(Int)+1,UInt(2)) == BigRational(typemax(Int),-2) == -BigRational(typemin(Int)+1,-2)
    @test BigRational(-4,Int8(-4)) == BigRational(typemax(UInt32), typemax(UInt32)) == Int16(1)
    @test BigRational(Int16(3),Int8(3)) == BigRational(UInt(1),1) == BigRational(Int16(4),0x4) == ONE
    @test BigRational(-3,UInt(5)) == BigRational(UInt16(6),-10) == -BigRational(-3,-5)
    @test BigRational(UInt(14),-7) == BigRational(typemax(UInt)-1,-typemax(Int)) == BigRational(0x6,-3) == BigRational(-2,1)
    @test BigRational(0x0007,0x7) == BigRational(0x9,9) == BigRational(BigInt(1),true) == one(BigInt)
    @test BigRational(typemin(Int16),-2) == cld(typemax(Int16),2)
    @test BigRational(-3,typemin(Int16)) == BigRational(3,typemax(Int16)+1)
    @test BigRational(typemin(Int32), UInt(3)) == BigRational(typemin(Int32)%Int64, UInt(3)) == BigRational(typemin(Int32), UInt16(3))
    @test BigRational(typemin(Int64), UInt(3)) == BigRational(typemin(Int64)%Int64, UInt(3)) == BigRational(typemin(Int64), UInt16(3))
    @test BigRational(typemin(Int), typemin(Int)) == -BigRational(-3,3) == -BigRational(7,-7) == ONE
    @test BigRational(3,false) == BigRational(true,false) == BigRational(true, 0) == BigRational(Int128(1),Int128(0))
    @test BigRational(true, 12) == 1//12
    @test iszero(BigRational(false, 0x1))

    @test BigRational(1.5) == BigRational(3,2) == 1.5
    @test BigRational(Float16(ℯ)) == Float16(ℯ)
    @test BigRational(BigFloat(13.25)) == BigRational(53,4) == 13.25
end

@testset "Rational functions" begin
    unsigned_types_test = (UInt8, UInt32, UInt64)
    for i in 1:3, j in i:3
        y1 = unsigned_types_test[i](9); y2 = unsigned_types_test[j](6)
        z = BigRational(y1, y2)
        @test numerator(z) == 3
        @test denominator(z) == 2
        if i != j
            z = BigRational(y2, y1)
            @test numerator(z) == 2
            @test denominator(z) == 3

        end
    end
    signed_types_test = (Int8, Int32, Int64, BigInt)
    for i in 1:4, j in i:4
        x1 = signed_types_test[i](12); x2 = signed_types_test[j](-4)
        z = BigRational(x1, x2)
        @test numerator(z) == -3
        @test denominator(z) == 1
        if i != j
            z = BigRational(x2, x1)
            @test numerator(z) == -1
            @test denominator(z) == 3
            for k in 1:3, t in i:3
                y1 = unsigned_types_test[k](3); y2 = unsigned_types_test[t](2)
                z = BigRational(x1, y1)
                @test numerator(z) == 4
                @test denominator(z) == 1
                z = BigRational(y1,x1)
                @test numerator(z) == 1
                @test denominator(z) == 4
                z = BigRational(x2, y1)
                @test numerator(z) == -4
                @test denominator(z) == 3
                z = BigRational(y1,x2)
                @test numerator(z) == -3
                @test denominator(z) == 4
                if k != t
                    z = BigRational(x1, y2)
                    @test numerator(z) == 6
                    @test denominator(z) == 1
                    z = BigRational(y2,x1)
                    @test numerator(z) == 1
                    @test denominator(z) == 6
                    z = BigRational(x2, y2)
                    @test numerator(z) == -2
                    @test denominator(z) == 1
                    z = BigRational(y2,x2)
                    @test numerator(z) == -1
                    @test denominator(z) == 2
                end
            end
        end
    end

    @test Rational(BigRational(3,4)) == Rational{BigInt}(BigRational(3,4)) == 3//4
    @test Rational{Int16}(BigRational(typemax(Int32),typemax(Int32))) === one(Rational{Int16})
    @test isinteger(BigRational(5))
    @test !isinteger(BigRational(3,4))
end

@testset "Conversions and promotions" begin
    @test BigFloat(BigRational(3,4)) == float(BigRational(3,4)) == Float16(BigRational(3,4)) == 0.75
    @test Bool(one(BigRational)) == !Bool(zero(BigRational)) == Int8(one(BigRational)) == true
    @test_throws InexactError Bool(BigRational(3,4))
    @test_throws InexactError Int(BigRational(3,4))
    @test promote(3//4, BigRational(2,1)) == promote(BigRational(3,4), 2) == (BigRational(3,4), BigRational(2))

    x1 = promote(BigRational(1,2), 0.75)
    x2 = promote(Float16(0.5), BigRational(3,4))
    x3 = (BigFloat(0.5), BigFloat(0.75))
    @test (x1[1] ≈ x2[1] ≈ x3[1]) && (x1[2] ≈ x2[2] ≈ x3[2])
end

@testset "IO operations" begin
    @test string(BigRational(6,3)) == "BigRational(2,1)"
    @test string(BigRational[BigRational(-2,3), BigRational(4,5)]) == "BigRational[-2//3, 4//5]"
    io = IOBuffer()
    show(io, BigRational(2,-4))
    @test String(take!(io)) == "BigRational(-1,2)"

    io = IOBuffer()
    # The following tests are broken because there is no read/write for BigInt in Base
    @test_broken write(io, BigRational(2,3)) == 32
    @test_broken read(io, BigRational) == BigRational(2,3)
end

@testset "Comparisons" begin
    @test BigRational(3,4) == BigRational(6,8) == Rational{Int}(3,4) == Rational{UInt}(3,4)
    @test BigRational(2,3) != BigRational(4,7) != 2//5
    @test 0x3 == BigRational(3) == 3
    @test Int16(5) != BigRational(3) != UInt(7)
    @test big(-4) == BigRational(-4) == Int128(-4)
    @test Int128(12) != BigRational(-4) != big(4)
    @test zero(BigRational) == false
    @test iszero(zero(BigRational))
    @test !iszero(typemin(BigRational))
    @test true == one(BigRational)
    @test isone(one(BigRational))
    @test !isone(typemax(BigRational))
    @test Rational{Int128}(7,9) == BigRational(7,9) != Rational{Bool}(true, true)

    @test false < BigRational(3,4) < BigRational(4,2) < 3
    @test BigRational(2,3) >= 2//3 >= BigRational(2,3)
    @test Rational{UInt}(3,5) <= BigRational(3,5) <= Rational{BigInt}(3,5)
    @test BigInt(1) <= BigRational(2,2) < 0x2
    @test Int128(3) <= BigRational(23,7) < Rational{Int128}(25,7)
    @test Inf >= BigRational(4,5) > 0.3
    @test !(BigRational(3,5) > NaN) && !(BigRational(4,7) < NaN) && !(BigRational(5,6) == NaN)

    @test isfinite(BigRational(-4,3))
    @test !isfinite(BigRational(2,0))
    @test !isfinite(BigRational(-3,0))
end

@testset "Arithmetic operations" begin
    @test numerator(BigRational(typemax(Int),typemin(Int))) == -typemax(Int)
    @test numerator(-BigRational(typemax(Int),typemin(Int))) == typemax(Int)
    for op in (:+, :-, :*, :/, ://, :div, :rem, :mod, :fld, :cld)
        ret1 = @eval $op(47//7, 13//5)
        ret2 = @eval $op(15//2, 26//11)
        for T in (:(BigRational), :(Rational{Int16}), :(Rational{UInt}), :(Rational{Int128}), :(Rational{BigInt}))
            @test @eval $op(BigRational(47,7), $T(13,5)) == $ret1
            @test @eval $op($T(15,2), BigRational(26,11)) == $ret2
        end
    end
    @test rem(BigRational(100,3), 2) == BigRational(100,3) - 2 * div(BigRational(100,3), 2)
    @test rem(5, BigRational(2,7)) == 5 - BigRational(2,7) * div(5, BigRational(2,7))
    @test fld(BigRational(100,3), 2) == div(BigRational(100,3), 2) == cld(BigRational(100,3), 2) - 1
    @test cld(-5, BigRational(2,7)) == div(-5, BigRational(2,7)) == fld(-5, BigRational(2,7)) + 1
    @test 4^(BigRational(3,2)) == 2.0^(BigRational(3,1)) == 8.0
    @test im^(BigRational(3,2)) ≈ -sqrt(2)/2 + sqrt(2)*im/2
    @test (BigRational(2,3)*im)^true == (2im)//3 == (BigRational(-3,2)*im)^(-1)
    @test (BigRational(1,1)*im)^false == BigRational(2,3)^0
    @test BigRational(4,9)^3 == BigRational(64,729)^1 == BigRational(8,27)^2
    @test BigRational(16,25)^(-1) == BigRational(25,16) == BigRational(4,5)^(-2)
    x = 2 # used to avoid ^ to Val lowering
    @test BigRational(16,25)^(x-3) == BigRational(25,16) == BigRational(4,5)^(-x)
    @test BigRational(2,3)^8 == BigRational(4,9)^4 == BigRational(16,81)^2
    @test ℯ^BigRational(3,4) ≈ 2.117
    for op in (:-, :inv)
        @test @eval $op(BigRational(12,7)) == $op(12//7)
        @test @eval $op(BigRational(-2,3)) == $op(-2//3)
    end
    for op in (:trunc, :floor, :ceil, :round)
        @test @eval $op(UInt, BigRational(14,5)) == $op(UInt, 14//5)
        for T in (:(Rational{Int8}), :Int, :(Union{BigInt,Missing}), :Float16)
            @test @eval $op($T, BigRational(45,13)) == $op($T, 45//13)
            @test @eval $op($T, BigRational(-7,9)) == $op($T, -7//9)
        end
    end
    @test round(Float64, typemax(BigRational)) === -round(Float64, typemin(BigRational)) === Inf
    @test round(BigRational(5,2), RoundUp) == BigRational(3)
    @test round(Union{Int,Missing}, BigRational(3,2), RoundDown) == round(Union{Int,Missing}, BigRational(3,2), RoundToZero)
    @test round(Union{Float64,Missing}, BigRational(1,4), RoundUp) == 1.0
    @test_throws DivideError round(Int, typemax(BigRational))
    if VERSION >= v"1.4.0-rc1"
        @test gcd(BigRational(3,4), BigRational(5,6)) == gcd(3//4, 5//6)
        @test lcm(BigRational(-2,7), BigRational(3,8)) == lcm(-2//7, 3//8)
        @test gcdx(BigRational(1,9), BigRational(5,-2)) == gcdx(1//9, -5//2)
        @test gcdx(BigRational(2,3), zero(BigRational)) == gcdx(2//3, 0//1)
        @test gcdx(BigRational(1,0), BigRational(5,3)) == BigRational.(gcdx(1//0, 5//3))
    end
    @test sign(BigRational(-3,4)) == -sign(BigRational(2,9))
    @test signbit(BigRational(5,-8)) && !signbit(BigRational(3,4))
    @test copysign(BigRational(2,3), -4.3) == copysign(BigRational(-4,6), BigRational(-1,3)) == -2//3
    @test copysign(BigRational(-4,5), BigInt(2)) == copysign(BigRational(4,5), 2//3) == BigRational(4,5)
    @test flipsign(BigRational(1,3), -3.0)  == flipsign(BigRational(2,6), -1) == flipsign(BigRational(-1,3), true) == -1//3
    @test flipsign(BigRational(2,5), 0x2) == copysign(BigRational(-2,5), 0x0003) == BigRational(2,5)
    @test abs(BigRational(typemin(Int)+2,typemax(Int))) == abs(BigRational(typemax(Int)-1,typemax(Int)))
end

@testset "Other functions" begin
    x = BigRational(3,4)
    @test widen(BigRational) == BigRational
    @test widen(x) isa BigRational
    @test big(BigRational) == BigRational
    @test big(x) === x
    @test Base.hastypemax(BigRational)
    @test denominator(typemax(BigRational)) == denominator(typemin(BigRational)) == 0
    @test numerator(typemax(BigRational)) == -numerator(typemin(BigRational)) == 1
    @test fma(x, x, x) == x*(x+1)
    @test fma(BigRational(2,3), BigRational(6,5), BigRational(12,7)) == 2//3 * 6//5 + 12//7
    y = deepcopy(x)
    BigRationals.MPQ.mul_2exp!(y, 3)
    @test x == 3//4
    @test y == x * (1<<3)
    @test hash(x) == hash(3//4)
    @test hash(y, UInt(123)) == hash(x*(1<<3), UInt(123))
    z = [BigRational(7,5)]
    push!(z, BigRationals.MPQ.div_2exp(first(z), 2))
    push!(z, first(z))
    t = deepcopy(z)
    @test first(t) === last(t)
    @test first(t) != t[2]
    @test hash(t) == hash(z)
    @test t == z == BigRational[7//5, 7//20, 7//5]
    BigRationals.MPQ.swap!(z[1], z[3])
    @test hash(t) == hash(z)
    @test t == z == BigRational[7//5, 7//20, 7//5]
    @test first(t) === last(t)
    BigRationals.MPQ.swap!(z[1], z[2])
    @test hash(t) != hash(z)
    @test t != z
    @test z == BigRational[7//20, 7//5, 7//20]
    @test first(t) === last(t)
end
