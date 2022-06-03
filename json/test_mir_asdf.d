/+ dub.sdl:
name "json_d_mir_asdf"
targetPath "target"
dependency "asdf" version="~>0.7.15"
dependency "mir_common_json" path="mir-common-json"
dflags "-mcpu=native" "-linkonce-templates" "-enable-cross-module-inlining" platform="ldc"
+/
import asdf : deserialize;
import mir_common_json;
import mir.stdio;
import std.file : read;

auto calc(string text)
{
    return text.deserialize!Avg.coordinates.avg;
}

void main()
{
    validate!calc;
    auto text = cast(string) "/tmp/1.json".read;
    "D/ldc2 (Mir Asdf DOM)".notifyStart;
    auto coordinate = calc(text);
    notifyStop;
    coordinate.writeln;
}
