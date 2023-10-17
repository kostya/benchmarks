/+ dub.sdl:
name "json_d_mir_ion"
targetPath "target"
dependency "mir-ion" version="~>2.2.0"
dependency "mir_common_json" path="mir-common-json"
dflags "-mcpu=native" "-linkonce-templates" "-enable-cross-module-inlining" platform="ldc"
+/
import mir_common_json;
import mir.deser.json : deserializeJson;
import mir.stdio : writeln;
import std.file : read;

auto calc(scope const(char)[] text) @nogc @safe pure
{
    return text.deserializeJson!Avg.coordinates.avg;
}

void main()
{
    validate!calc;
    auto text = cast(string) "/tmp/1.json".read;
    "D/ldc2 (Mir Amazon's Ion DOM)".notifyStart;
    auto coordinate = calc(text);
    notifyStop;
    coordinate.writeln;
}
