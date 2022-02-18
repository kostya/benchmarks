/+ dub.sdl:
name "json_d_mir_ion_file"
targetPath "target"
dependency "mir-ion" version="~>1.0.26"
dependency "mir_common_json" path="mir-common-json"
dflags "-mcpu=native" "-linkonce-templates" "-enable-cross-module-inlining" platform="ldc"
+/
import mir_common_json;
import mir.ion.deser.json : deserializeJsonFile;

void main() @safe @nogc
{
    "D/ldc2 (Mir Amazon's Ion DOM, file input)".notifyStart;
    auto coordinate = "/tmp/1.json".deserializeJsonFile!Avg.coordinates.avg;
    notifyStop;
    coordinate.print;
}
