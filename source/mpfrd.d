module mpfrd;

import mpfr;
import std.traits;

struct Mpfr {
    mpfr_t mpfr;
    alias mpfr this;

    @disable this(this);

    this(int precision) {
        mpfr_init2(mpfr, precision);
    }

    ~this() {
        mpfr_clear(mpfr);
    }

    ref Mpfr opAssign(T)(T value) if(isNumeric!T) {
        static if (is(T == double)) {
            mpfr_set_d(mpfr, value, mpfr_rnd_t.MPFR_RNDN);
        } else static if (is(T == float)) {
            mpfr_set_flt(mpfr, value, mpfr_rnd_t.MPFR_RNDN);
        } else static if (isIntegral!T && isSigned!T) {
            mpfr_set_si(mpfr, value, mpfr_rnd_t.MPFR_RNDN);
        } else static if (isIntegral!T && !isSigned!T) {
            mpfr_set_ui(mpfr, value, mpfr_rnd_t.MPFR_RNDN);
        } else {
            static assert(false, "Unhandled type "~T.stringof);
        }
        return this;
    }

    int opCmp(T)(T value) if(isNumeric!T) {
        static if (is(T : double)) {
            return mpfr_cmp_d(mpfr, value);
        } else static if (isIntegral!T && isSigned!T) {
            return mpfr_cmp_si(mpfr, value);
        } else static if (isIntegral!T && !isSigned!T) {
            return mpfr_cmp_ui(mpfr, value);
        } else {
            static assert(false, "Unhandled type "~T.stringof);
        }
    }

    bool opEquals(T)(T value) if(isNumeric!T) {
        return opCmp(value) == 0;
    }
}

version (unittest)
{
    import std.meta;
    import std.stdio : writeln, writefln;
}

unittest {
    auto a = Mpfr(8);
    foreach(T ; AliasSeq!(ubyte, ushort, uint, ulong, float, double, byte, short, int, long)) {
        static if (isSigned!T) {
            a = T(-1);
            assert(a == T(-1));
            assert(a <= T(-1));
            assert(a > T(-2));
            assert(a < T(0));
        } else {
            a = T(1);
            assert(a == T(1));
            assert(a <= T(1));
            assert(a < T(2));
            assert(a > T(0));
        }
    }
}