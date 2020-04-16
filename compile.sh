mkdir -p build
echo -n "#define COMMIT_ID \"" > build/commitid.h
git rev-parse --short HEAD | tr -d '\n' >> build/commitid.h
echo "\"" >> build/commitid.h
echo -n "#define VERSION_STRING \"" >> build/commitid.h
git describe --abbrev=0 2>/dev/null | tr -d '\n' >> build/commitid.h
echo "\"" >> build/commitid.h
ghc Main.hs -o main
