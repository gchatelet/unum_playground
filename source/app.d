import std.stdio;

import utils;
import pegged.grammar;

/// Numbers
mixin(grammar(`
Numbers:
    Special    <~ "nan" / ( Sign? "inf" ) / Scientific
    Scientific <~ Floating ( ('e' / 'E' ) Integer )? ('...')?
    Floating   <~ Integer ('.' Unsigned )?
    Unsigned   <~ [0-9]+
    Integer    <~ Sign? Unsigned
    Hexa       <~ [0-9a-fA-F]+
    Sign       <- '-' / '+'
`));

alias U = Unum!(3, 4);

void interpret(string line) {
    auto value = Numbers(line);
    if(value.matches != [line]) {
        writefln(`Error parsing '%s'`, value.matches);
        return;
    }
    import std.conv : to;
    const real_value = to!real(line);
    writeln(x2u!U(real_value ).toDebugString);
}

version(unittest) {} else
void main(string[] args)
{
    import std.stdio : readln;
    import std.string : strip;
    for(;;) {
        interpret(strip(readln()));
    }
}
