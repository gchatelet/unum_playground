// import std.stdio;
// import std.string;
// import deimos.mpfr;
// import std.bitmanip;
// 
// string floatString(bool sign, ulong fraction, ushort exponent, int fsizemax, int esizemax) {
//     with(mpfr_rnd_t) {
//         enum precision = 128;
// 
//         auto ratio = Mpfr(precision);
//         mpfr_ui_pow_ui(ratio, 2, fsizemax, MPFR_RNDN);
// 
//         auto frac = Mpfr(precision);
//         mpfr_set_ui(frac, fraction, MPFR_RNDN);
//         mpfr_div(frac, frac, ratio, MPFR_RNDN);
//         if(exponent) mpfr_add_ui(frac, frac, 1, MPFR_RNDN);
// 
//         auto magnitude = Mpfr(precision);
//         mpfr_set_si(magnitude, unbias(esizemax, exponent), MPFR_RNDN);
//         mpfr_ui_pow(magnitude, 2, magnitude, MPFR_RNDN);
// 
//         auto result = Mpfr(precision);
//         mpfr_mul(result, magnitude, frac, MPFR_RNDN);
//         if (sign) mpfr_neg(result, result, MPFR_RNDN);
// 
//         return floatString(result);
//     }
// }
// 
// struct Bound {
//     enum fsizemax = fraction.sizeof * 8; // 64 bits
//     enum esizemax = exponent.sizeof * 8; // 16 bits
//     bool sign;
//     ulong fraction;
//     ushort exponent;
//     bool open;
//     bool inf;
// 
//     pure @nogc nothrow static Bound infinity(bool sign) {
//         return Bound(sign, 0, 0, false, true);
//     }
// 
//     pure @nogc nothrow static Bound from(U)(in U value, bool open) if (IsUnum!U) {
//         assert(!isNaN(value));
//         assert(isExact(value));
//         assert(fsizemax >= U.fsizemax);
//         assert(esizemax >= U.esizemax);
//         auto f_ratio = 2^^(Bound.fsizemax - U.fsizemax);
//         with(value) {
//             if(isFinite(value)) {
//                 if(fraction == 0) {
//                     return Bound(sign, 0, 0, open, false);
//                 } else {
//                     const unbiased = unbias(U.esizemax, exponent);
//                     auto biased = bias(Bound.esizemax, unbiased);
//                     assert(biased < ushort.max);
//                     assert(biased > 0);
//                     auto new_fraction = fraction * f_ratio;
//                     if(exponent == 0) {
// 
//                         --biased;
//                     }
//                     return Bound(sign, new_fraction, cast(ushort)(biased), open, false);
//                 }
//             } else {
//                 return Bound(sign, 0, 0, open, true);
//             }
//         }
//     }
// }
// 
// struct GBound {
//     alias fsizemax = Bound.fsizemax;
//     alias esizemax = Bound.esizemax;
// 
//     bool nan;
//     Bound left;
//     Bound right;
// 
//     pure @nogc nothrow static GBound from(U)(in U value) if (IsUnum!U) {
//         if (isNaN(value)) return GBound(true);
//         if (isExact(value)) {
//             auto bound = Bound.from(value, false);
//             return GBound(false, bound, bound);
//         }
//         if (value is posopeninfu!U) {
//             return GBound(false, Bound.from(value.previous, true), Bound.infinity(false));
//         }
//         if (value is negopeninfu!U) {
//             return GBound(false, Bound.infinity(true), Bound.from(value.next, true));
//         }
//         if (value.sign) {
//             return GBound(false, Bound.from(value.next, true), Bound.from(value.previous, true));
//         } else {
//             return GBound(false, Bound.from(value.previous, true), Bound.from(value.next, true));
//         }
//     }
// }