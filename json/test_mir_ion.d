/+ dub.sdl:
name "test_mir_ion"
dependency "mir-ion" version="~>0.1.11"
dependency "mir_common_json" path="mir-common-json"
dflags "-mcpu=native" "-linkonce-templates" "-enable-cross-module-inlining" platform="ldc"
lflags "-L../common/libnotify/target/"
+/
import mir_common_json;
import mir.ion.deser.json: deserializeJson;
import std.file: read;

auto calc(string text) @nogc
{
    return text.deserializeJson!Avg.coordinates.avg;
}

int main()
{
    auto text = cast(string) "/tmp/1.json".read;

    foreach (v; [
        `{"coordinates":[{"x":2.0,"y":0.5,"z":0.25}]}`,
        `{"coordinates":[{"y":0.5,"x":2.0,"z":0.25}]}`])
        if (calc(v) != Coordinate(2.0, 0.5, 0.25))
            return 1;

    "D/ldc (Mir Amazon's Ion)".notifyStart;
    auto coordinate = calc(text);
    notifyStop;
    coordinate.print;
    return 0;
}
