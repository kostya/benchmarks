/+ dub.sdl:
name "json_d_mir_ion"
targetPath "target"
dependency "mir-ion" version="~>0.1.67"
dependency "mir_common_json" path="mir-common-json"
dflags "-mcpu=native" "-linkonce-templates" "-enable-cross-module-inlining" platform="ldc"
+/
import mir_common_json;
import mir.ion.deser.json : deserializeJson;
import std.file : read;

auto calc(string text) @nogc @safe pure
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
    coordinate.print;
}
