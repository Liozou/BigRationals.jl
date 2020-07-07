module BigRationals

import Base.GMP: Limb

export BigRational

"""
    BigRational <: Real

Arbitrary precision rational type.
"""
mutable struct BigRational <: Real
    num_alloc::Cint
    num_size::Cint
    num_d::Ptr{Limb}
    den_alloc::Cint
    den_size::Cint
    den_d::Ptr{Limb}

    function BigRational()
        b = MPQ.init!(new())
        finalizer(cglobal((:__gmpq_clear, :libgmp)), b)
        return b
    end
end

include("MPQ.jl")
include("functions.jl")

end
