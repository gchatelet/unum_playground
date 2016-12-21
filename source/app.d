import std.stdio;

import utils;
import pegged.grammar;

/// Numbers
mixin(grammar(`
Numbers:
    Scientific <~ Floating ( ('e' / 'E' ) Integer )? ('...')?
    Special    <~ 'nan' / (Sign? 'inf')
    Floating   <~ Integer ('.' Unsigned )?
    Unsigned   <~ [0-9]+
    Integer    <~ Sign? Unsigned
    Hexa       <~ [0-9a-fA-F]+
    Sign       <- '-' / '+'
`));

alias U = Unum!(3, 5);

void interpret(string line) {
    auto value = Numbers(line);
    if(value.matches != [line]) {
        writefln(`Error parsing '%s'`, line);
        return;
    }
    import std.conv : to;
    const real_value = to!real(line);
    writeln(x2u!U(real_value ).toHumanString);
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
