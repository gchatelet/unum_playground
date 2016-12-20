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


nothrow @nogc @property
{

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

string humanString(alias display = floatString, U)(in U value) if (IsUnum!U) {
    if(isNaN(value)) return value.sign ? "sNaN" : "qNaN";
    if(isInfinity(value)) return value.sign ? "-Inf" : "Inf";
    if(!isExact(value)) {
        string a = humanString!(display, U)(value.previous);
        string b = humanString!(display, U)(value.next);
        return format("(%s, %s)", value.sign ? b : a, value.sign ? a : b);
    }
    with(value) {
        return display(sign, fraction, exponent, fsizemax, esizemax);
    }
}

string humanString(alias display = floatString)(in GBound value) {
    if (value.nan) return "nan";
    if (value.left == value.right) {
        return humanString!display(value.left);
    }
    return format("(%s, %s)", humanString!display(value.left), humanString!display(value.right));
}

string humanString(alias display = floatString)(in Bound value) {
    with (value) {
        if (inf) {
            return sign ? "-Inf" : "Inf";
        } else {
            return display(sign, fraction, exponent, fsizemax, esizemax);
        }
    }
}

string debugString(bool sign, ulong fraction, ushort exponent, int fsizemax, int esizemax) {
    string hidden = exponent > 0 ? "1+" : "";
    string prefix = sign ? "-" : "";
    import std.bigint;
    BigInt a = "2";
    auto b = a ^^ fsizemax;
    return format("%s2^%dx(%s%d/%s) (%d %d %d %d) (%s)",
        prefix, unbias(esizemax, exponent), hidden, fraction, b.toDecimalString,
        fraction, exponent, fsizemax, esizemax,
        floatString(sign, fraction, exponent, fsizemax, esizemax));
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
        mpfr_set_si(magnitude, unbias(esizemax, exponent), MPFR_RNDN);
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

TO convert(FROM, TO)(in FROM input) if(IsUnum!FROM && IsUnum!TO) {

}

unittest {
// main.Unum!(0, 1).Unum 2^1x(1/4) (1 0 2 1) (0.5)
// main.Unum!(1, 0).Unum 2^0x(1/2) (1 0 1 2) (0.5)
// main.Unum!(1, 1).Unum 2^0x(2/4) (2 0 2 2) (0.5)
// main.Unum!(2, 2).Unum 2^-1x(1+0/16) (0 6 4 4) (0.5)

// main.Unum!(0, 1).Unum 2^1x(2/4) (2 0 2 1) (1)
// main.Unum!(1, 0).Unum 2^0x(1+0/2) (0 1 1 2) (1)
// main.Unum!(1, 1).Unum 2^0x(1+0/4) (0 1 2 2) (1)
// main.Unum!(2, 2).Unum 2^0x(1+0/16) (0 7 4 4) (1)

// main.Unum!(1, 1).Unum 2^0x(3/4) (3 0 2 2) (0.75)
// main.Unum!(2, 2).Unum 2^-1x(1+8/16) (8 6 4 4) (0.75)

// main.Unum!(0, 1).Unum 2^1x(3/4) (3 0 2 1) (1.5)
// main.Unum!(1, 0).Unum 2^0x(1+1/2) (1 1 1 2) (1.5)
// main.Unum!(1, 1).Unum 2^0x(1+2/4) (2 1 2 2) (1.5)
// main.Unum!(2, 2).Unum 2^0x(1+8/16) (8 7 4 4) (1.5)

// core.bitop.bsr

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
        auto f_ratio = 2^^(Bound.fsizemax - U.fsizemax);
        with(value) {
            if(isFinite(value)) {
                if(fraction == 0) {
                    return Bound(sign, 0, 0, open, false);
                } else {
                    const unbiased = unbias(U.esizemax, exponent);
                    auto biased = bias(Bound.esizemax, unbiased);
                    assert(biased < ushort.max);
                    assert(biased > 0);
                    auto new_fraction = fraction * f_ratio;
                    if(exponent == 0) {

                        --biased;
                    }
                    return Bound(sign, new_fraction, cast(ushort)(biased), open, false);
                }
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

//               f e f e
// 2^1x(1/4)    (1 0 2 1) (0.5)
// 2^0x(1/2)    (1 0 1 2) (0.5)
// 2^0x(2/4)    (2 0 2 2) (0.5)
// 2^-1x(1+0/2) (0 6 1 4) (0.5)
// 2^-1x(1+0/4) (0 6 2 4) (0.5)

//    2^0x(3/4) (3 0 2 2) (0.75)
// 2^-1x(1+1/2) (1 6 1 4) (0.75)
// 2^-1x(1+2/4) (2 6 2 4) (0.75)

//   2^1x(2/4)  (2 0 2 1) (1)
//  2^0x(1+0/2) (0 1 1 2) (1)
//  2^0x(1+0/4) (0 1 2 2) (1)
//  2^0x(1+0/2) (0 7 1 4) (1)
//  2^0x(1+0/4) (0 7 2 4) (1)

//    2^1x(3/4) (3 0 2 1) (1.5)
//  2^0x(1+1/2) (1 1 1 2) (1.5)
//  2^0x(1+2/4) (2 1 2 2) (1.5)
//  2^0x(1+1/2) (1 7 1 4) (1.5)
//  2^0x(1+2/4) (2 7 2 4) (1.5)