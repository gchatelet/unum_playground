import std.stdio;

import utils;
import pegged.grammar;

/// Numbers
mixin(grammar(`
Numbers:
    Scientific <~ Floating ( ('e' / 'E' ) Integer )? ('...')?
    Floating   <~ Integer ('.' Unsigned )?
    Unsigned   <~ [0-9]+
    Integer    <~ Sign? Unsigned
    Hexa       <~ [0-9a-fA-F]+
    Sign       <- '-' / '+'
`));

void interpret(string line) {
    auto value = Numbers(line);
    if(value.matches != [line]) {
        writefln(`Error parsing '%s'`, line);
    }
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
