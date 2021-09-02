#!/usr/bin/env python3
# build.py
# This is the script that has 3 responsibilities:
#  - generates important static data in build directory that is used at compile-time
#  - runs the build system (ie stack)
#  - installs the resulting executables
# what it does is controlled by 4 flags + optional dev flag:
#  - ./build.py configure just runs the configure step.
#  - ./build.py build runs  the build system.
#  - ./build.py install  installs ire appropriately.
#  - ./build.py unistall is the inverse of install.
#  note ./build.py build is not typically used for compiler development, instead, 
#  stack build, stack run -- [option], and stack test are used directly.
#  (and runtime needs to be compiled beforehand with cd runtime && make)
# note that install will change configure settings so that irec will look for stdlib
# in system locations rather than this directory.
# ALSO: user ./build.py configure debug to configure ire to use local directories 
# if you want to develop on ire.


import sys
import platform
import shutil
import subprocess
import os

opsys = ""
# why startswith? see https://docs.python.org/3/library/sys.html#sys.platform
if sys.platform.startswith("linux"):
    opsys = "linux"
elif sys.platform == "win32":
    opsys = "windows"
elif sys.platform == "darwin":
    opsys = "macos"
else:
    opsys = fail("Error Determining Operating System", "(linux/windows/macos)")

mach = platform.machine()

if mach == "x86_64":
    arch = "amd64"
elif mach == "i386":
    arch = "i386"
elif mach == "aarch64":
    arch = "aarch64"
elif mach == "aarch32":
    arch = "aarch32"
else:
    arch = fail("Error Identifying Arch", "(amd64/i386/aarch32/aarch64)")

IRELIBS_PATH = "/usr/lib/ire/"
STDLIB_PATH = IRELIBS_PATH + "stdlib/"
LINUX_AMD64_LIB_PATH = IRELIBS_PATH + "irert_linux_amd64.o"
LINUX_AARCH64_LIB_PATH = IRELIBS_PATH + "irert_linux_aarch64.o"
BINARY_PATH = "/usr/bin/irec"

hasfailed = 0

def fail(msg, val):
    global hasfailed
    hasfailed = 1
    print(msg+"\n")
    return val

def run_version():
    output = subprocess.run(["git", "rev-parse", "--short", "HEAD"], capture_output=True).stdout
    commit = output.decode("utf-8").replace("\n", "")
    output = subprocess.run(["git", "describe",  "--abbrev=0"], capture_output=True).stdout
    tag = output.decode("utf-8").replace("\n", "")
    stuff = {
        "COMMIT_ID" : commit,
        "VERSION_STRING" : tag,
    }
    with open("build/commitid.h", "w") as fd:
        fd.write("#ifndef __COMMITID_H__\n")
        fd.write("#define __COMMITID_H__\n")
        for key, value in stuff.items():
            fd.write("#define " + key + " \"" + value + "\"\n")
        fd.write("#endif")
        fd.close()
def run_config(debug):

    
    linux_linker = shutil.which("ld.lld")
    if linux_linker == None:
        linux_linker = fail("Error Finding ld.lld on path", "path/to/ld")
    opt = shutil.which("opt")
    if opt == None:
        opt = fail("Error Finding opt on path", "path/to/opt")
    llc = shutil.which("llc")
    if llc == None:
        llc = fail("Error Finding llc on path", "path/to/llc")
    stdlib_path = os.path.abspath("stdlib") if debug else STDLIB_PATH
    linux_amd64_lib_path = os.path.abspath("bin/irert_linux_amd64.o") if debug else LINUX_AMD64_LIB_PATH
    linux_aarch64_lib_path = os.path.abspath("bin/irert_linux_aarch64.o") if debug else LINUX_AARCH64_LIB_PATH
    config = {
        "HOST_SYSTEM" : opsys + "-" + arch,
        "LINUX_LINKER_PATH" : linux_linker,
        "LLC_PATH" : llc,
        "OPT_PATH" : opt,
        "STDLIB_PATH": stdlib_path,
        "LINUX_AMD64_LIB_PATH" : linux_amd64_lib_path,
        "LINUX_AARCH64_LIB_PATH" : linux_aarch64_lib_path
        }
    
    if hasfailed:
        print("There were error(s) determining configuration. edit \"build/config.h\" manually or rerun the script\n")
        exit(1)
    
    with open("build/config.h", "w") as fd:
        fd.write("#ifndef __CONFIG_H__\n")
        fd.write("#define __CONFIG_H__\n")
        for key, value in config.items():
            fd.write("#define " + key + " \"" + value + "\"\n")
        fd.write("#endif")
        fd.close()

def run_build(debug):
    make = shutil.which("make")
    if make == None:
        print("Error: could not find make")
        exit(1)
    os.chdir("runtime")
    subprocess.run([make, "clean"])
    subprocess.run([make])
    os.chdir("../")
    stack = shutil.which("stack")
    if stack == None:
        print("Error: could not find stack.")
        exit(1)
    subprocess.run([stack, "--allow-different-user", "clean"])
    if (debug):
        subprocess.run([stack, "--allow-different-user", "build"])
    else:
        subprocess.run([stack, "--allow-different-user", "build", "--ghc-options", "-O3"])
    subprocess.run([stack, "--allow-different-user", "install", "--local-bin-path", "bin"])
    

def run_install():
    try:
        try:
            os.mkdir(IRELIBS_PATH)
        except FileExistsError:
            print("An existing installation already exists. try running uninstall first.")
            exit(1)
        shutil.copytree("stdlib", STDLIB_PATH)
        shutil.copy2("bin/irert_linux_amd64.o", LINUX_AMD64_LIB_PATH)
        shutil.copy2("bin/irert_linux_aarch64.o", LINUX_AARCH64_LIB_PATH)
        shutil.copy2("bin/irec", BINARY_PATH)
        print("Done installing")
    except PermissionError:
        print("The script does not have permission to copy files. try running with elevated priveledges.")
        exit(1)

def run_uninstall():
    try:
        shutil.rmtree(IRELIBS_PATH)
        os.remove(BINARY_PATH)
    except PermissionError:
        print("The script does not have permission to copy files. try running with elevated priveledges.")
        exit(1)

if __name__ == "__main__":
    if (len(sys.argv)) < 2 or not (sys.argv[1] in ["configure", "build", "install", "uninstall"] ):
        print("Usage: " + sys.argv[0] + " [configure/build/install]")
        exit(1)
    opt = sys.argv[1]
    debug = False
    if(len(sys.argv) >= 3 and sys.argv[2] == "dev"):
        print("Using development settings")
        debug = True
    try:
        os.mkdir("build")
    except FileExistsError:
        pass
    if opt == "configure":
        run_version()
        run_config(debug)
        print("Done writing config. run build now")
    elif opt == "build":
        run_build(debug)
        print("Done building. run install now.")
    elif opt == "install":
        run_install()
    elif opt == "uninstall":
        run_uninstall()
