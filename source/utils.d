module utils;

import std.math;

// Returns the number of hardware bits to use to store v bits.
pure nothrow @nogc uint hardwareBits(uint v) {
    assert(v <= 64);
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

unittest {
    assert(hardwareBits(0) == 0);
    assert(hardwareBits(1) == 8);
    assert(hardwareBits(4) == 8);
    assert(hardwareBits(12) == 16);
    assert(hardwareBits(23) == 32);
}

// Defines the prototype for a Unum.
template IsUnum(U) {
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

// Fnum for fat num.
struct Fnum {
    enum esizemax = exponent.sizeof * 8; // 16 bits
    enum fsizemax = fraction.sizeof * 8; // 64 bits
    enum exponent_max = exponent.max;
    enum fraction_max = fraction.max;

    bool ubit;
    ulong fraction;
    ushort exponent;
    bool sign;
}

struct Unum(int e, int f) {
    import std.bitmanip;

    enum esizemax = 2 ^^ e;
    enum fsizemax = 2 ^^ f;
    // enum exponent_max = exponent.max;
    // enum fraction_max = fraction.max;

    enum esizesize = e;
    enum fsizesize = f;
    enum storage_bits = 1 /*ubit*/  + esizemax + fsizemax + 1 /* sign */ ;
    enum hardware_bits = hardwareBits(storage_bits);
    enum storage_lost_bits = hardware_bits - storage_bits;

    union {
        struct {
            mixin(bitfields!(
                bool, "ubit", 1,
                ulong, "fraction", fsizemax,
                ushort, "exponent", esizemax,
                bool, "sign", 1,
                ulong, "unused", storage_lost_bits));
        }
        struct {
            mixin(bitfields!(
                ulong, "raw", hardware_bits));
        }
    }


    string toString() const {
        import std.format;
        return format("%X", raw);
    }
}

version (unittest)
{
    import std.meta;
    import std.stdio : writeln, writefln;

    alias UnumTypes = AliasSeq!(Unum!(0, 0), Unum!(1, 1), Unum!(2, 3), Unum!(3, 4), Fnum);
}

unittest {
    foreach (U; UnumTypes) {
        scope(failure) writeln("Error with type : ", typeid(U));
        assert(IsUnum!U);
    }
}

// Constructs an Unum of type U with specified values.
pure nothrow @nogc auto build(U)(bool sign, uint exponent, ulong fraction, bool ubit) if (IsUnum!U) {
    U unum;
    unum.sign = sign;
    unum.exponent = cast(ushort) exponent;
    unum.fraction = fraction;
    unum.ubit = ubit;
    return unum;
}

unittest {
    foreach (U; UnumTypes) {
        scope(failure) writeln("Error with type : ", typeid(U));
        auto value = build!U(true, 1, 1, true);
        assert(value.sign == true);
        assert(value.exponent == 1);
        assert(value.fraction == 1);
        assert(value.ubit == true);
    }
}

template qNaNu(U)           { enum qNaNu =           build!U(false, U.exponent_max, U.fraction_max, true); }
template sNaNu(U)           { enum sNaNu =           build!U(true,  U.exponent_max, U.fraction_max, true); }
template posinfu(U)         { enum posinfu =         build!U(false, U.exponent_max, U.fraction_max, false); }
template neginfu(U)         { enum neginfu =         build!U(true,  U.exponent_max, U.fraction_max, false); }
template posopeninfu(U)     { enum posopeninfu =     build!U(false, U.exponent_max, U.fraction_max - 1, true); }
template negopeninfu(U)     { enum negopeninfu =     build!U(true,  U.exponent_max, U.fraction_max - 1, true); }
template poszerou(U)        { enum poszerou =        build!U(false, 0, 0, false); }
template negzerou(U)        { enum negzerou =        build!U(true,  0, 0, false); }
template posopenzerou(U)    { enum posopenzerou =    build!U(false, 0, 0, true); }
template negopenzerou(U)    { enum negopenzerou =    build!U(true,  0, 0, true); }
template maxrealu(U)        { enum maxrealu =        build!U(false, U.exponent_max, U.fraction_max - 1, false); }
template minrealu(U)        { enum minrealu =        build!U(true,  U.exponent_max, U.fraction_max - 1, false); }
template smallsubnormalu(U) { enum smallsubnormalu = build!U(false, 0, 1, false); }
template smallnormalu(U)    { enum smallnormalu    = build!U(false, 1, 0, false); }

bool isExact(U)(in U value)        if (IsUnum!U) { return !value.ubit; }
bool isNotFinite(U)(in U value)    if (IsUnum!U) { return value.fraction == U.fraction_max && value.exponent == U.exponent_max; }
bool isFinite(U)(in U value)       if (IsUnum!U) { return !isNotFinite(value); }
bool isNaN(U)(in U value)          if (IsUnum!U) { return isNotFinite(value) && value.ubit; }
bool isSignalingNaN(U)(in U value) if (IsUnum!U) { return isNaN(value) && value.sign; }
bool isQuietNaN(U)(in U value)     if (IsUnum!U) { return isNaN(value) && !value.sign; }
bool isInfinity(U)(in U value)     if (IsUnum!U) { return isNotFinite(value) && !value.ubit; }
bool isPositive(U)(in U value)     if (IsUnum!U) { return !isNaN(value) && !value.sign; }
bool isNegative(U)(in U value)     if (IsUnum!U) { return !isNaN(value) && value.sign; }

unittest {
    foreach (U; UnumTypes) {
        scope(failure) writeln("Error with type : ", typeid(U));
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


auto allValues(U)() if (IsUnum!U) {
    import std.algorithm.setops : cartesianProduct;
    import std.algorithm.iteration : map;
    import std.range : iota;
    immutable signs = [false, true];
    immutable ubits = [false, true];
    return cartesianProduct(signs, iota(U.exponent_max + 1), iota(U.fraction_max + 1), ubits)
        .map!( t => build!U(t[0],t[1],t[2],t[3]) );
}

auto allFinite(U)() if (IsUnum!U) {
    import std.algorithm.iteration : filter;
    return allValues!U.filter!(x => isFinite(x));
}

auto allFiniteExact(U)() if (IsUnum!U) {
    import std.algorithm.iteration : filter;
    return allFinite!U.filter!(x => isExact(x));
}

pure @nogc nothrow int unbias(uint esizemax, ushort biased_exponent) {
    assert(esizemax > 0);
    assert(esizemax <= 16);
    assert(2^^esizemax > biased_exponent);
    immutable uint esizemaxminus1 = esizemax - 1;
    immutable uint bias = 2^^esizemaxminus1 - 1;
    return biased_exponent ?
        biased_exponent - bias :
        biased_exponent - bias + 1;
}

unittest {
    assert(unbias(1, 0) == 1);
    assert(unbias(1, 1) == 1);
    assert(unbias(8, 0) == -126);
    assert(unbias(8, 1) == -126);
    assert(unbias(8, 2) == -125);
    assert(unbias(8, 255) == 128);
    assert(unbias(16, 0) == -32766);
    assert(unbias(16, 1) == -32766);
    assert(unbias(16, 2) == -32765);
    assert(unbias(16, 65535) == 32768);
}

pure @nogc nothrow uint bias(uint esizemax, int exponent) {
    assert(esizemax > 0);
    assert(esizemax <= 16);
    immutable uint esizemaxminus1 = esizemax - 1;
    immutable int bias = 2^^esizemaxminus1 - 1;
    return exponent + bias;
}

unittest {
    assert(bias(1, 1) == 1);
    assert(bias(8, -126) == 1);
    assert(bias(8, -125) == 2);
    assert(bias(8, 128) == 255);
    assert(bias(16, -32766) == 1);
    assert(bias(16, -32765) == 2);
    assert(bias(16, 32768) == 65535);
}

pure @nogc real toReal(U)(in U value) if (IsUnum!U) {
    static assert(U.esizemax <= 15);
    static assert(U.fsizemax <= 63);
    assert(isExact(value));
    assert(isFinite(value));
    with(value) {
        real frac = fraction * 2.^^-fsizemax;
        if (exponent) frac += 1;
        real magnitude = 2.^^unbias(esizemax, exponent);
        real result = magnitude * frac;
        return sign ? -result : result;
    }
}

string toHumanString(U)(in U value) if (IsUnum!U) {
    if(isNaN(value)) return value.sign ? "sNaN" : "qNaN";
    if(isInfinity(value)) return value.sign ? "-Inf" : "Inf";
    import std.format;
    if(!isExact(value)) {
        string a = toHumanString(value.previous);
        string b = toHumanString(value.next);
        return format("(%s, %s)", value.sign ? b : a, value.sign ? a : b);
    }
    return format("%g", toReal(value));
}


string toBinString(U)(in U value) if (IsUnum!U) {
    import std.format;
    with(value) return format("%.1b %.*b %.*b %.1b", sign, esizemax, exponent, fsizemax, fraction, ubit);
}

string toMathString(U)(in U value) if (IsUnum!U) {
    import std.format;
    with(value) return format("%s2^%dx(%d/%d%s)%s",
            sign ? "-" : "",
            unbias(U.esizemax, exponent),
            fraction,
            fraction_max,
            exponent > 0 ? "+1" : "",
            ubit ? "..." : "");
}

string toDebugString(U)(in U value) if (IsUnum!U) {
    import std.format;
    return format("%s | %20s | %s",
        toBinString(value),
        toMathString(value),
        toHumanString(value)
    );
}

// Finds the scale factor.
pure nothrow @nogc int scale(real x) {
    return x == 0 ? 0 : cast(uint)(floor(log2(abs(x))));
}

unittest {
    assert(scale(0) == 0);
    assert(scale(1) == 0);
    assert(scale(2) == 1);
    assert(scale(4) == 2);
    assert(scale(10) == 3);
    assert(scale(-10) == 3);
    assert(scale(-4) == 2);
    assert(scale(-2) == 1);
    assert(scale(-1) == 0);
}

// Conversion of a floatable real to a unum. Most of the complexity stems from
// seeking the shortest possible bit string.
U x2u(U)(real x) if (IsUnum!U) {
    import std.math;
    if (std.math.isNaN(x))
        return qNaNu!U;
    if (std.math.isInfinity(x))
        return signbit(x) ? neginfu!U : posinfu!U;
    if (abs(x) > maxrealu!U.toReal)
        return signbit(x) ? negopeninfu!U : posopeninfu!U;
    if (x == 0)
        return signbit(x) ? negzerou!U : poszerou!U;
    if (abs(x) < smallsubnormalu!U.toReal)
        return signbit(x) ? negopenzerou!U : posopenzerou!U;
    immutable smallnormal = smallnormalu!U.toReal;
    if (abs(x) < smallnormal) {
        immutable smallsubnormal = smallsubnormalu!U.toReal;
        auto fraction = abs(x) / smallsubnormal;
        auto fraction_floor = floor(fraction);
        return build!U(x < 0, 0, cast(ulong)fraction_floor, fraction_floor != fraction);
    } else {
        auto scaled = abs(x) / 2.^^scale(x);
        const uint biased_exponent = bias(U.esizemax, scale(x));
        const fraction = 2.^^U.fsizemax * (scaled - 1);
        auto fraction_floor = floor(fraction);
        return build!U(x < 0, biased_exponent, cast(ulong)fraction_floor, fraction_floor != fraction);
    }
}

unittest {
    alias Walpiri = Unum!(0, 0);
    assert(x2u!Walpiri(0) == poszerou!Walpiri);

    // Too big
    assert(x2u!Walpiri(10) == posopeninfu!Walpiri); // 10 > 2
    assert(x2u!Walpiri(-4) == negopeninfu!Walpiri); // -4 < -2

    // Too small
    assert(x2u!Walpiri(0.2) == posopenzerou!Walpiri); // 0.2 < 1
    assert(x2u!Walpiri(-.2) == negopenzerou!Walpiri); // -.2 < -1

    // NaN
    assert(x2u!Walpiri(real.nan) == qNaNu!Walpiri);

    // Infinities
    assert(x2u!Walpiri(real.infinity) == posinfu!Walpiri);
    assert(x2u!Walpiri(-real.infinity) == neginfu!Walpiri);

    foreach(U ; AliasSeq!(Unum!(0, 0), Unum!(0, 1), Unum!(1, 0), Unum!(1, 1), Unum!(2, 2))) {
        foreach(x ; allFinite!U) {
            if (isNotFinite(x)) continue;
            real floatable;
            if (isExact(x)) {
                floatable = toReal(x);
            } else if (x == posopeninfu!U ) {
                floatable = toReal(maxrealu!U) + 1;
            } else if (x == negopeninfu!U ) {
                floatable = toReal(minrealu!U) - 1;
            } else {
                floatable = (toReal(x.previous) + toReal(x.next)) / 2;
            }
            assert(x2u!U(floatable) == x);
        }
    }
}
