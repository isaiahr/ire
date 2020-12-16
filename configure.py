# configure.py
# This is the configure script that generates important static data in build directory that is used at compile-time
#
#
#
#
#
#
#
import sys
import platform
import shutil

hasfailed = 0

def fail(msg, val):
    global hasfailed
    hasfailed = 1
    print(msg+"\n")
    return val


def main(debug):
    os = ""
    # why startswith? see https://docs.python.org/3/library/sys.html#sys.platform
    if sys.platform.startswith("linux"):
        os = "linux"
    elif sys.platform == "win32":
        os = "windows"
    elif sys.platform == "darwin":
        os = "macos"
    else:
        os = fail("Error Determining Operating System" "(linux/windows/macos)")
    
    mach = platform.machine()
    
    if mach == "x86_64":
        arch = "amd64"
    elif mach == "i386":
        arch = "i386"
    else:
        arch = fail("Error Identifying Arch", "(amd64/i386/aarch32/aarch64)")
    
    linux_linker = shutil.which("ld.lld")
    if linux_linker == None:
        linux_linker = fail("Error Finding ld.lld on path" "path/to/ld")
    opt = shutil.which("opt")
    if opt == None:
        opt = fail("Error Finding opt on path" "path/to/opt")
    llc = shutil.which("llc")
    if llc == None:
        llc = fail("Error Finding llc on path" "path/to/llc")
    
    config = {
        "HOST_SYSTEM" : os + "-" + arch,
        "LINUX_LINKER_PATH" : linux_linker,
        "LLC_PATH" : llc,
        "OPT_PATH" : opt,
        "LINUX_AMD64_LIB_PATH" : "build/irert_linux_amd64.o",
        }
    
    if hasfailed:
        print("There were error(s) determining configuration. edit \"build/config.h\" manually or rerun the script\n")
    
    with open("build/config.h", "w") as fd:
        fd.write("#ifndef __CONFIG_H__\n")
        fd.write("#define __CONFIG_H__\n")
        for key, value in config.items():
            fd.write("#define " + key + " \"" + value + "\"\n")
        fd.write("#endif")





if __name__ == "__main__":
    main(True);
