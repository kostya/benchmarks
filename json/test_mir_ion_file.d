/+ dub.sdl:
name "test_mir_ion_file"
dependency "mir-ion" version="~>0.1.11"
dependency "mir_common_json" path="mir-common-json"
dflags "-mcpu=native" "-linkonce-templates" "-enable-cross-module-inlining" platform="ldc"
lflags "-L../common/libnotify/target/"
+/
import mir_common_json;
import mir.ion.deser.json: deserializeJsonFile;

int main()
{
    "D/ldc/Mir (Mir Amazon's Ion, file input)".notifyStart;
    auto coordinate = "/tmp/1.json".deserializeJsonFile!Avg.coordinates.avg;
    notifyStop;
    coordinate.print;
    return 0;
}
