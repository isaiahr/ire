if [ "$1" = clean ]
then
    # dont glob, will fail with more than n files
    rm -rv bin
    mkdir -p bin
    exit 0
fi

FLAGS=""

if [ "$1" = prod ]
then
    FLAGS="-O3"
fi

if [ "$1" = dbg ]
then
    FLAGS="-g -prof -fprof-auto"
fi

mkdir -p bin
mkdir -p build
echo -n "#define COMMIT_ID \"" > build/commitid.h
git rev-parse --short HEAD | tr -d '\n' >> build/commitid.h
echo "\"" >> build/commitid.h
echo -n "#define VERSION_STRING \"" >> build/commitid.h
git describe --abbrev=0 2>/dev/null | tr -d '\n' >> build/commitid.h
echo "\"" >> build/commitid.h
ghc $FLAGS Main.hs -hidir bin -odir bin -o main
