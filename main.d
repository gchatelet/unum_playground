import std.stdio;
import std.string;
import mpfr;
import std.bitmanip;

struct Mpfr
{
    mpfr_t value;
    alias value this;

    this(int precision)
    {
        mpfr_init2(value, precision);
    }

    ~this()
    {
        mpfr_clear(value);
    }
}

// pure nothrow @nogc @safe int scale(double value)
// in {
//     assert(isFinite(value));
// } body {
//     return value == 0 ? 0 : cast(int)log2(abs(value));
// }

// unittest {
//     assert(scale(0) == 0);
//     assert(scale(1) == 0);
//     assert(scale(-8.6) == 3);
//     assert(scale(0.03125) == -5);
// }

// pure nothrow @nogc @safe uint ne(double value)
// in {
//     assert(isFinite(value));
// } body {
//     if (value == 0 || scale(value) == 1) return 1;
//     return cast(uint)ceil(log2(1 + abs(scale(value) - 1))) + 1;
// }

// unittest {
//     assert(ne(0) == 1);
//     assert(ne(1) == 2);
//     assert(ne(8.6) == 3);
//     assert(ne(0.03125) == 4);
// }

pure nothrow @nogc @safe uint hardwareBits(ulong v)
{
    if (v > 32)
        return 64;
    if (v > 16)
        return 32;
    if (v > 8)
        return 16;
    if (v > 0)
        return 8;
    return 0;
}

unittest
{
    assert(hardwareBits(0) == 0);
    assert(hardwareBits(1) == 8);
    assert(hardwareBits(4) == 8);
    assert(hardwareBits(12) == 16);
    assert(hardwareBits(23) == 32);
}

// Fnum for fat num.
struct Fnum
{
    enum esizemax = exponent.sizeof * 8; // 16 bits
    enum fsizemax = fraction.sizeof * 8; // 64 bits
    enum exponent_max = exponent.max;
    enum fraction_max = fraction.max;

    bool ubit;
    ulong fraction;
    ushort exponent;
    bool sign;
}

struct Unum(int e, int f)
{
    enum esizemax = 2 ^^ e;
    enum fsizemax = 2 ^^ f;
    // enum exponent_max = exponent.max;
    // enum fraction_max = fraction.max;

    enum esizesize = e;
    enum fsizesize = f;
    enum storage_bits = 1 /*ubit*/  + esizemax + fsizemax + 1 /* sign */ ;
    enum hardware_bits = hardwareBits(storage_bits);
    enum storage_lost_bits = hardware_bits - storage_bits;

    mixin(bitfields!(bool, "ubit", 1, //
            ulong, "fraction", fsizemax, //
            ushort, "exponent", esizemax, //
            bool, "sign", 1, //
            ulong, "unused", storage_lost_bits));
}

unittest
{
    with (Unum!(0, 0))
    {
        assert(storage_bits == 4);
        assert(hardware_bits == 8);
    }
    with (Unum!(2, 1))
    {
        assert(storage_bits == 8);
        assert(hardware_bits == 8);
    }
    with (Unum!(2, 3))
    {
        assert(storage_bits == 14);
        assert(hardware_bits == 16);
    }
    with (Unum!(3, 4))
    {
        assert(storage_bits == 26);
        assert(hardware_bits == 32);
    }
}

template IsUnum(U)
{
    enum IsUnum = is(typeof((inout int = 0) {
                uint esizemax = U.esizemax; // maximum number of bits of the exponent.
                uint fsizemax = U.fsizemax; // maximum number of bits of the fraction.
                ushort e_max = U.exponent_max; // maximum value of the exponent.
                ulong f_max = U.fraction_max; // maximum value of the fraction.

                U u = U.init; // can define an instance
                bool sign = u.sign; // has a sign
                ulong fraction = u.fraction; // has a fraction
                ushort exponent = u.exponent; // has an exponent
                bool ubit = u.ubit; // has a ubit
            }));
}

version (unittest)
{
    import std.meta;

    alias UnumTypes = AliasSeq!(Unum!(0, 0), Unum!(1, 1), Unum!(3, 4), Fnum);
}

unittest
{
    foreach (U; UnumTypes)
    {
        static assert(IsUnum!U);
    }
}

nothrow @nogc auto build(U)(bool sign, uint exponent, ulong fraction, bool ubit)
        if (IsUnum!U)
{
    U unum;
    unum.sign = sign;
    unum.exponent = cast(ushort) exponent;
    unum.fraction = fraction;
    unum.ubit = ubit;
    return unum;
}

unittest
{
    foreach (U; UnumTypes)
    {
        auto value = build!U(true, 1, 1, true);
        assert(value.sign == true);
        assert(value.exponent == 1);
        assert(value.fraction == 1);
        assert(value.ubit == true);
    }
}

nothrow @nogc @property
{
    bool isExact(U)(in U value) if (IsUnum!U)
    {
        return !value.ubit;
    }

    bool isNotFinite(U)(in U value) if (IsUnum!U)
    {
        return value.fraction == U.fraction_max && value.exponent == U.exponent_max;
    }

    bool isFinite(U)(in U value) if (IsUnum!U)
    {
        return !isNotFinite(value);
    }

    bool isNaN(U)(in U value) if (IsUnum!U)
    {
        return isNotFinite(value) && value.ubit;
    }

    bool isSignalingNaN(U)(in U value) if (IsUnum!U)
    {
        return isNaN(value) && value.sign;
    }

    bool isQuietNaN(U)(in U value) if (IsUnum!U)
    {
        return isNaN(value) && !value.sign;
    }

    bool isInfinity(U)(in U value) if (IsUnum!U)
    {
        return isNotFinite(value) && !value.ubit;
    }

    bool isPositive(U)(in U value) if (IsUnum!U)
    {
        return !isNaN(value) && !value.sign;
    }

    bool isNegative(U)(in U value) if (IsUnum!U)
    {
        return !isNaN(value) && value.sign;
    }

    auto qNaNu(U)() if (IsUnum!U)
    {
        return build!U(false, U.exponent_max, U.fraction_max, true);
    }

    unittest
    {
        foreach (U; UnumTypes)
        {
            immutable value = qNaNu!U;
            assert(!value.isExact);
            assert(!value.isFinite);
            assert(!value.isInfinity);
            assert(value.isNaN);
            assert(!value.isNegative);
            assert(!value.isPositive);
            assert(value.isQuietNaN);
            assert(!value.isSignalingNaN);
        }
    }

    auto sNaNu(U)() if (IsUnum!U)
    {
        return build!U(true, U.exponent_max, U.fraction_max, true);
    }

    unittest
    {
        foreach (U; UnumTypes)
        {
            immutable value = sNaNu!U;
            assert(!value.isExact);
            assert(!value.isFinite);
            assert(!value.isInfinity);
            assert(value.isNaN);
            assert(!value.isNegative);
            assert(!value.isPositive);
            assert(!value.isQuietNaN);
            assert(value.isSignalingNaN);
        }
    }

    auto posinfu(U)() if (IsUnum!U)
    {
        return build!U(false, U.exponent_max, U.fraction_max, false);
    }

    unittest
    {
        foreach (U; UnumTypes)
        {
            immutable value = posinfu!U;
            assert(value.isExact);
            assert(!value.isFinite);
            assert(value.isInfinity);
            assert(!value.isNaN);
            assert(!value.isNegative);
            assert(value.isPositive);
            assert(!value.isQuietNaN);
            assert(!value.isSignalingNaN);
        }
    }

    auto neginfu(U)() if (IsUnum!U)
    {
        return build!U(true, U.exponent_max, U.fraction_max, false);
    }

    unittest
    {
        foreach (U; UnumTypes)
        {
            immutable value = neginfu!U;
            assert(value.isExact);
            assert(!value.isFinite);
            assert(value.isInfinity);
            assert(!value.isNaN);
            assert(value.isNegative);
            assert(!value.isPositive);
            assert(!value.isQuietNaN);
            assert(!value.isSignalingNaN);
        }
    }

    auto posopeninfu(U)() if (IsUnum!U)
    {
        return build!U(false, U.exponent_max, U.fraction_max - 1, true);
    }

    unittest
    {
        foreach (U; UnumTypes)
        {
            immutable value = posopeninfu!U;
            assert(!value.isExact);
            assert(value.isFinite);
            assert(!value.isInfinity);
            assert(!value.isNaN);
            assert(!value.isNegative);
            assert(value.isPositive);
            assert(!value.isQuietNaN);
            assert(!value.isSignalingNaN);
        }
    }

    auto negopeninfu(U)() if (IsUnum!U)
    {
        return build!U(true, U.exponent_max, U.fraction_max - 1, true);
    }

    unittest
    {
        foreach (U; UnumTypes)
        {
            immutable value = negopeninfu!U;
            assert(!value.isExact);
            assert(value.isFinite);
            assert(!value.isInfinity);
            assert(!value.isNaN);
            assert(value.isNegative);
            assert(!value.isPositive);
            assert(!value.isQuietNaN);
            assert(!value.isSignalingNaN);
        }
    }

    auto poszerou(U)() if (IsUnum!U)
    {
        return build!U(false, 0, 0, false);
    }

    unittest
    {
        foreach (U; UnumTypes)
        {
            immutable value = poszerou!U;
            assert(value.isExact);
            assert(value.isFinite);
            assert(!value.isInfinity);
            assert(!value.isNaN);
            assert(!value.isNegative);
            assert(value.isPositive);
            assert(!value.isQuietNaN);
            assert(!value.isSignalingNaN);
        }
    }

    auto negzerou(U)() if (IsUnum!U)
    {
        return build!U(true, 0, 0, false);
    }

    unittest
    {
        foreach (U; UnumTypes)
        {
            immutable value = negzerou!U;
            assert(value.isExact);
            assert(value.isFinite);
            assert(!value.isInfinity);
            assert(!value.isNaN);
            assert(value.isNegative);
            assert(!value.isPositive);
            assert(!value.isQuietNaN);
            assert(!value.isSignalingNaN);
        }
    }

    auto posopenzerou(U)() if (IsUnum!U)
    {
        return build!U(false, 0, 0, true);
    }

    unittest
    {
        foreach (U; UnumTypes)
        {
            immutable value = posopenzerou!U;
            assert(!value.isExact);
            assert(value.isFinite);
            assert(!value.isInfinity);
            assert(!value.isNaN);
            assert(!value.isNegative);
            assert(value.isPositive);
            assert(!value.isQuietNaN);
            assert(!value.isSignalingNaN);
        }
    }

    auto negopenzerou(U)() if (IsUnum!U)
    {
        return build!U(true, 0, 0, true);
    }

    unittest
    {
        foreach (U; UnumTypes)
        {
            immutable value = negopenzerou!U;
            assert(!value.isExact);
            assert(value.isFinite);
            assert(!value.isInfinity);
            assert(!value.isNaN);
            assert(value.isNegative);
            assert(!value.isPositive);
            assert(!value.isQuietNaN);
            assert(!value.isSignalingNaN);
        }
    }

    auto maxrealu(U)() if (IsUnum!U)
    {
        return build!U(false, U.exponent_max, U.fraction_max - 1, false);
    }

    unittest
    {
        foreach (U; UnumTypes)
        {
            immutable value = maxrealu!U;
            assert(value.isExact);
            assert(value.isFinite);
            assert(!value.isInfinity);
            assert(!value.isNaN);
            assert(!value.isNegative);
            assert(value.isPositive);
            assert(!value.isQuietNaN);
            assert(!value.isSignalingNaN);
        }
    }
    
    auto minrealu(U)() if (IsUnum!U)
    {
        return build!U(true, U.exponent_max, U.fraction_max - 1, false);
    }
    
    unittest
    {
        foreach (U; UnumTypes)
        {
            immutable value = minrealu!U;
            assert(value.isExact);
            assert(value.isFinite);
            assert(!value.isInfinity);
            assert(!value.isNaN);
            assert(value.isNegative);
            assert(!value.isPositive);
            assert(!value.isQuietNaN);
            assert(!value.isSignalingNaN);
        }
    }
    
    auto smallsubnormalu(U)() if (IsUnum!U)
    {
        return build!U(false, 0, 1, false);
    }
    
    unittest
    {
        foreach (U; UnumTypes)
        {
            immutable value = smallsubnormalu!U;
            assert(value.isExact);
            assert(value.isFinite);
            assert(!value.isInfinity);
            assert(!value.isNaN);
            assert(!value.isNegative);
            assert(value.isPositive);
            assert(!value.isQuietNaN);
            assert(!value.isSignalingNaN);
        }
    }
    
    auto next(U)(in U value) if (IsUnum!U) {
        with(value) {
            if(!ubit) return build!U(sign, exponent, fraction, true);
            if(fraction < U.fraction_max) return build!U(sign, exponent, fraction + 1, false);
            if(exponent < U.exponent_max) return build!U(sign, exponent + 1, 0, false);
            if(!sign) return build!U(true, 0, 0, false);
            return build!U(false, 0, 0, false);
        }
    }

    auto previous(U)(in U value) if (IsUnum!U) {
        with(value) {
            if(ubit) return build!U(sign, exponent, fraction, false);
            if(fraction > 0) return build!U(sign, exponent, fraction - 1, true);
            if(exponent > 0) return build!U(sign, exponent - 1, U.fraction_max, true);
            if(sign) return build!U(false, U.exponent_max, U.fraction_max, true);
            return build!U(true, U.exponent_max, U.fraction_max, true);
        }
    }
    
    unittest
    {
        foreach (U; UnumTypes)
        {
            assert(poszerou!U.next is smallsubnormalu!U.previous);
            assert(sNaNu!U.next is poszerou!U);
            // add more tests
        }
    }
}

int expoValue(U)(in U value) if (IsUnum!U) in {
    assert(isFinite(value));
} body {
    with(value) {
        immutable uint esizemaxminus1 = esizemax - 1;
        immutable int bias = 2^^esizemaxminus1 - 1;
        return exponent ? exponent - bias : exponent - bias + 1;
    }
}

int expoValue(uint esizemax, ushort exponent) {
    immutable uint esizemaxminus1 = esizemax - 1;
    immutable int bias = 2^^esizemaxminus1 - 1;
    return exponent ? exponent - bias : exponent - bias + 1;
}

string humanString(U)(in U value) if (IsUnum!U) {
    if(isNaN(value)) return value.sign ? "sNaN" : "qNaN";
    if(isInfinity(value)) return value.sign ? "-Inf" : "Inf";
    if(!isExact(value)) {
        string a = value.previous.humanString();
        string b = value.next.humanString();
        return format("(%s, %s)", value.sign ? b : a, value.sign ? a : b);
    }
    with(value) {
        return floatString(sign, fraction, exponent, fsizemax, esizemax);
    }
}

string humanString(in GBound value) {
    if (value.nan) return "nan";
    if (value.left is value.right) {
        return value.left.humanString();
    }
    return format("(%s, %s)", value.left.humanString(), value.right.humanString());
}

string humanString(in Bound value) {
    with (value) {
        if (inf) {
            return sign ? "-Inf" : "Inf";
        } else {
            writeln(fraction, " ",exponent, " ", fsizemax, " ", esizemax);
            return floatString(sign, fraction, exponent, fsizemax, esizemax);
        }
    }
}

string floatString(bool sign, ulong fraction, ushort exponent, int fsizemax, int esizemax) {
    with(mpfr_rnd_t) {
        enum precision = 128;

        auto ratio = Mpfr(precision);
        mpfr_ui_pow_ui(ratio, 2, fsizemax, MPFR_RNDN);

        auto frac = Mpfr(precision);
        mpfr_set_ui(frac, fraction, MPFR_RNDN);
        mpfr_div(frac, frac, ratio, MPFR_RNDN);
        if(exponent) mpfr_add_ui(frac, frac, 1, MPFR_RNDN);

        auto magnitude = Mpfr(precision);
        mpfr_set_si(magnitude, expoValue(esizemax, exponent), MPFR_RNDN);
        mpfr_ui_pow(magnitude, 2, magnitude, MPFR_RNDN);

        auto result = Mpfr(precision);
        mpfr_mul(result, magnitude, frac, MPFR_RNDN);
        if (sign) mpfr_neg(result, result, MPFR_RNDN);

        return floatString(result);
    }
}

string floatString(ref const Mpfr value) {
    char[1024] buf;
    auto count = mpfr_snprintf(buf.ptr, buf.sizeof, "%Rg".ptr, &value);
    return buf[0..count].idup;
}

auto allvalues(U)() if (IsUnum!U) {
    import std.algorithm.setops : cartesianProduct;
    import std.algorithm.iteration : map;
    import std.range : iota;
    immutable signs = [false, true];
    immutable ubits = [false, true];
    return cartesianProduct(signs, iota(U.exponent_max + 1), iota(U.fraction_max + 1), ubits)
        .map!( t => build!U(t[0],t[1],t[2],t[3]) );
}

struct Bound {
    enum fsizemax = fraction.sizeof * 8; // 64 bits
    enum esizemax = exponent.sizeof * 8; // 16 bits
    bool sign;
    ulong fraction;
    ushort exponent;
    bool open;
    bool inf;
    
    pure @nogc nothrow static Bound infinity(bool sign) {
        return Bound(sign, 0, 0, false, true);
    }
    
    pure @nogc nothrow static Bound from(U)(in U value, bool open) if (IsUnum!U) {
        assert(!isNaN(value));
        assert(isExact(value));
        assert(fsizemax >= U.fsizemax);
        assert(esizemax >= U.esizemax);
        auto f_ratio = fsizemax / U.fsizemax;
        auto e_ratio = esizemax / U.esizemax;
        with(value) {
            if(isFinite(value)) {
                assert(exponent * e_ratio <= ushort.max);
                return Bound(sign, fraction * f_ratio, cast(ushort)(exponent * e_ratio), open, false);
            } else {
                return Bound(sign, 0, 0, open, true);
            }
        }
    }
}

struct GBound {
    alias fsizemax = Bound.fsizemax;
    alias esizemax = Bound.esizemax;

    bool nan;
    Bound left;
    Bound right;
    
    pure @nogc nothrow static GBound from(U)(in U value) if (IsUnum!U) {
        if (isNaN(value)) return GBound(true);
        if (isExact(value)) {
            auto bound = Bound.from(value, false);
            return GBound(false, bound, bound);
        }
        if (value is posopeninfu!U) {
            return GBound(false, Bound.from(value.previous, true), Bound.infinity(false));
        }
        if (value is negopeninfu!U) {
            return GBound(false, Bound.infinity(true), Bound.from(value.next, true));
        }
        if (value.sign) {
            return GBound(false, Bound.from(value.next, true), Bound.from(value.previous, true));
        } else {
            return GBound(false, Bound.from(value.previous, true), Bound.from(value.next, true));
        }
    }
}

void main() {
    import std.meta;

    foreach(U; AliasSeq!(Unum!(0, 0), Unum!(1, 1), Unum!(3, 4), Fnum)) {
        writeln(typeid(U), " ", U.esizemax, " ", U.fsizemax);
    }

    import std.algorithm.iteration : map;
    alias U = Unum!(0, 0);
    foreach(value ; allvalues!U) {
        writefln("% 30s : %s", value.humanString, GBound.from(value).humanString);
    }
}