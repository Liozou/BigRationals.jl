module MPQ

import Base.GMP.MPZ: mpz_t
import ..BigRationals: BigRational

const mpq_t = Ref{BigRational}

gmpq(op::Symbol) = (Symbol(:__gmpq_, op), :libgmp)

for (op) in (:canonicalize, :init, :clear)
    op! = Symbol(op, :!)
    @eval $op!(x::BigRational) = (ccall($(gmpq(op)), Cvoid, (mpq_t,), x); x)
end

for (op, T) in (:set_ui => Culong, :set_si => Clong)
    op! = Symbol(op, :!)
    @eval begin
        $op!(x::BigRational, a, b) = (ccall($(gmpq(op)), Cvoid, (mpq_t, $T, Culong), x, a, b); x)
        $op(a, b) = $op!(BigRational(), a, b)
    end
end

for (op, T) in (:set => mpq_t, :set_z => mpz_t, :set_d => Cdouble, :set_num => mpz_t, :set_den => mpz_t)
    op! = Symbol(op, :!)
    @eval $op!(x::BigRational, a) = (ccall($(gmpq(op)), Cvoid, (mpq_t, $T), x, a); x)
end

for op in (:get_num, :get_den)
    op! = Symbol(op, :!)
    @eval begin
        $op!(x::BigInt, a::BigRational) = (ccall($(gmpq(op)), Cvoid, (mpz_t, mpq_t), x, a); x)::BigInt
        $op(a::BigRational) = $op!(BigInt(), a)
    end
end



for (op) in (:inv, :neg, :abs)
    op! = Symbol(op, :!)
    @eval begin
        $op!(x::BigRational, a::BigRational) = (ccall($(gmpq(op)), Cvoid, (mpq_t, mpq_t), x, a); x)
        $op!(x::BigRational) = $op!(x, x)
    end
end

for op in (:set, :set_z, :set_d, :set_num, :inv, :neg, :abs)
    op! = Symbol(op, :!)
    @eval $op(a) = $op!(BigRational(), a)
end

for (op) in (:add, :sub, :mul, :div)
    op! = Symbol(op, :!)
    @eval begin
        $op!(x::BigRational, a::BigRational, b::BigRational) = (ccall($(gmpq(op)), Cvoid, (mpq_t, mpq_t, mpq_t), x, a, b); x)
        $op(a::BigRational, b::BigRational) = $op!(BigRational(), a, b)
        $op!(x::BigRational, b::BigRational) = $op!(x, x, b)
    end
end

for op in (:mul_2exp, :div_2exp)
    op! = Symbol(op, :!)
    @eval begin
        $op!(x::BigRational, a::BigRational, b) = (ccall($(gmpq(op)), Cvoid, (mpq_t, mpq_t, Culong), x, a, b); x)
        $op(a::BigRational, b) = $op!(BigRational(), a, b)
        $op!(x::BigRational, b) = $op!(x, x, b)
    end
end

cmp(a::BigRational, b::BigRational) = Int(ccall((:__gmpq_cmp, :libgmp), Cint, (mpq_t, mpq_t), a, b))
equal(a::BigRational, b::BigRational) = Bool(ccall((:__gmpq_equal, :libgmp), Cint, (mpq_t, mpq_t), a, b))

cmp_z(a::BigRational, b::BigInt) = Int(ccall((:__gmpq_cmp_z, :libgmp), Cint, (mpq_t, mpz_t), a, b))
cmp_ui(a::BigRational, b, c) = Int(ccall((:__gmpq_cmp_ui, :libgmp), Cint, (mpq_t, Culong, Culong), a, b, c))
cmp_si(a::BigRational, b, c) = Int(ccall((:__gmpq_cmp_si, :libgmp), Cint, (mpq_t, Clong, Culong), a, b, c))

get_d(a::BigRational) = ccall((:__gmpq_get_d, :libgmp), Cdouble, (mpq_t,), a)

get_str!(x, a, b::BigRational) = (ccall((:__gmpq_get_str, :libgmp), Ptr{Cchar}, (Ptr{Cchar}, Cint, mpq_t), x, a, b); x)
set_str!(x::BigRational, a, b) = Int(ccall((:__gmpq_set_str, :libgmp), Cint, (mpq_t, Ptr{UInt8}, Cint), x, a, b))

swap!(x::BigRational, y::BigRational) = ccall((:__gmpq_swap, :libgmp), Cvoid, (mpq_t, mpq_t), x, y)

end # module MPQ
