
all: compile

GHC_OPTS = -O2

bin/chapter1.exe: src/chapter1.hs
	ghc src/chapter1.hs $(GHC_OPTS) -o bin/chapter1.exe

bin/chapter2.exe: src/chapter2.hs
	ghc src/chapter2.hs $(GHC_OPTS) -o bin/chapter2.exe

bin/chapter3.exe: src/chapter3.hs
	ghc src/chapter3.hs $(GHC_OPTS) -o bin/chapter3.exe

bin/chapter4.exe: src/chapter4.hs
	ghc src/chapter4.hs $(GHC_OPTS) -o bin/chapter4.exe

bin/chapter5.exe: src/chapter5.hs
	ghc src/chapter5.hs $(GHC_OPTS) -o bin/chapter5.exe

bin/chapter6.exe: src/chapter6.hs
	ghc src/chapter6.hs $(GHC_OPTS) -o bin/chapter6.exe

bin/chapter7.exe: src/chapter7.hs
	ghc src/chapter7.hs $(GHC_OPTS) -o bin/chapter7.exe

bin/chapter8.exe: src/chapter8.hs
	ghc src/chapter8.hs $(GHC_OPTS) -o bin/chapter8.exe

compile: init bin/chapter1.exe bin/chapter2.exe bin/chapter3.exe bin/chapter4.exe bin/chapter5.exe bin/chapter6.exe bin/chapter7.exe bin/chapter8.exe

init:
	@if [ ! -d bin ]; then mkdir bin; fi

clean:
	rm -rf src/*.o src/*.hi src/*~ bin

run: compile
	bin/chapter1.exe
	bin/chapter2.exe
	bin/chapter3.exe
	bin/chapter4.exe
	bin/chapter5.exe
	bin/chapter6.exe
	bin/chapter7.exe
	bin/chapter8.exe
