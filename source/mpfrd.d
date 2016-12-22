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

    private static string getTypeString(T)() {
        static if (isIntegral!T && isSigned!T) {
            return "_si";
        } else static if (isIntegral!T && !isSigned!T) {
            return "_ui";
        } else static if (is(T : double)) {
            return "_d";
        } else static if (is(T == Mpfr)) {
            return "";
        } else {
            static assert(false, "Unhandled type " ~ T.stringof);
        }
    }

    // Assign

    ref Mpfr opAssign(T)(T value) if(isNumeric!T) {
        mixin("mpfr_set" ~ getTypeString!T() ~ "(mpfr, value, mpfr_rnd_t.MPFR_RNDN);");
        return this;
    }
    
    ref Mpfr opAssign(ref const Mpfr value) {
        mpfr_set(mpfr, value, mpfr_rnd_t.MPFR_RNDN);
        return this;
    }
    
    // Compare and equals

    int opCmp(T)(T value) const if(isNumeric!T) {
        mixin("return mpfr_cmp" ~ getTypeString!T() ~ "(mpfr, value);");
    }
    
    int opCmp(ref const Mpfr value) {
        return mpfr_cmp(mpfr, value);
    }

    bool opEquals(T)(T value) const if(isNumeric!T) {
        return opCmp(value) == 0;
    }
    
    bool opEquals(ref const Mpfr value) {
        return opCmp(value) == 0;
    }
    
    // Arithmetic
    private static string getOperatorString(string op)() {
        final switch(op) {
            case "+": return "_add";
            case "-": return "_sub";
            case "*": return "_mul";
            case "/": return "_div";
            case "^^": return "_pow";
        }
    }

    ref Mpfr opOpAssign(string op, T)(T value) if(isNumeric!T) {
        static assert(!(op == "^^" && isFloatingPoint!T), "no operator ^^= with floating point.");
        mixin("mpfr" ~ getOperatorString!op() ~ getTypeString!T() ~ "(mpfr, mpfr, value, mpfr_rnd_t.MPFR_RNDN);");
        return this;
    }

    ref Mpfr opOpAssign(string op)(ref const Mpfr value) {
        mixin("mpfr" ~ getOperatorString!op() ~ "(mpfr, mpfr, value, mpfr_rnd_t.MPFR_RNDN);");
        return this;
    }
}

version (unittest)
{
    import std.meta;
    import std.stdio : writeln, writefln;
}

unittest {
    auto value = Mpfr(8);
    foreach(T ; AliasSeq!(ubyte, ushort, uint, ulong, float, double, byte, short, int, long)) {
        {
            value = T(1);
            assert(value == T(1));
            assert(value <= T(1));
            assert(value < T(2));
            assert(value > T(0));
            value *= T(2);
            assert(value == T(2));
            value /= T(2);
            assert(value == T(1));
            value += T(1);
            assert(value == T(2));
            value -= T(1);
            assert(value == T(1));
        }
        static if (!isFloatingPoint!T) {
            value = T(2);
            value ^^= T(2);
            assert(value == T(4));
        }
        static if (isSigned!T) {
            value = T(-1);
            assert(value == T(-1));
            assert(value <= T(-1));
            assert(value > T(-2));
            assert(value < T(0));
        }
    }
    {
        value = -1;
        auto tmp = Mpfr(32);
        tmp = -1;
        assert(value == tmp);
        assert(value <= tmp);
        tmp = -2;
        assert(value > tmp);
        tmp = 0;
        assert(value < tmp);
        //
        value = 2;
        tmp = 2;
        value *= tmp;
        assert(value == 4);
        //
        value = 2;
        tmp = 2;
        value /= tmp;
        assert(value == 1);
        //
        tmp = 1;
        value = 2;
        value += tmp;
        assert(value == 3);
        //
        tmp = 1;
        value = 2;
        value -= tmp;
        assert(value == 1);
        //
        tmp = 1;
        value = 2;
        value *= tmp;
        assert(value == 2);
        //
        tmp = 2;
        value = 2;
        value /= tmp;
        assert(value == 1);
        //
        tmp = 2;
        value = 2;
        value ^^= tmp;
        assert(value == 4);
    }
}