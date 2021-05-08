/+ dub.sdl:
name "test_mir_asdf"
dependency "asdf" version="~>0.7.8"
dependency "mir_common_json" path="mir-common-json"
dflags "-mcpu=native" "-linkonce-templates" "-enable-cross-module-inlining" platform="ldc"
+/
import mir_common_json;
import asdf: deserialize;
import std.file: read;

auto calc(string text)
{
    return text.deserialize!Avg.coordinates.avg;
}

void main()
{
    validate!calc;
    auto text = cast(string) "/tmp/1.json".read;
    "D/ldc (Mir Asdf)".notifyStart;
    auto coordinate = calc(text);
    notifyStop;
    coordinate.print;
}
