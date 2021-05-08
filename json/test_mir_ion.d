/+ dub.sdl:
name "test_mir_ion"
dependency "mir-ion" version="~>0.1.11"
dependency "mir_common_json" path="mir-common-json"
dflags "-mcpu=native" "-linkonce-templates" "-enable-cross-module-inlining" platform="ldc"
+/
import mir_common_json;
import mir.ion.deser.json: deserializeJson;
import std.file: read;

auto calc(string text) @nogc @safe pure
{
    return text.deserializeJson!Avg.coordinates.avg;
}

void main()
{
    validate!calc;
    auto text = cast(string) "/tmp/1.json".read;
    "D/ldc (Mir Amazon's Ion)".notifyStart;
    auto coordinate = calc(text);
    notifyStop;
    coordinate.print;
}
