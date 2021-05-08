/+ dub.sdl:
name "test_mir_asdf"
dependency "asdf" version="~>0.7.8"
dependency "mir_common_json" path="mir-common-json"
dflags "-mcpu=native" "-linkonce-templates" "-enable-cross-module-inlining" platform="ldc"
lflags "-L../common/libnotify/target/"
+/
import mir_common_json;
import asdf: deserialize;
import std.file: read;

auto calc(string text)
{
    return text.deserialize!Avg.coordinates.avg;
}

int main()
{
    auto text = cast(string) "/tmp/1.json".read;

    foreach (v; [
        `{"coordinates":[{"x":2.0,"y":0.5,"z":0.25}]}`,
        `{"coordinates":[{"y":0.5,"x":2.0,"z":0.25}]}`])
        if (calc(v) != Coordinate(2.0, 0.5, 0.25))
            return 1;

    "D/ldc (Mir Asdf)".notifyStart;
    auto coordinate = calc(text);
    notifyStop;
    coordinate.print;
    return 0;
}


