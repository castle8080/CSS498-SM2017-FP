
all: compile

bin/chapter1.exe: src/chapter1.hs
	ghc src/chapter1.hs -o bin/chapter1.exe

bin/chapter2.exe: src/chapter2.hs
	ghc src/chapter2.hs -o bin/chapter2.exe

bin/chapter3.exe: src/chapter3.hs
	ghc src/chapter3.hs -o bin/chapter3.exe

bin/chapter4.exe: src/chapter4.hs
	ghc src/chapter4.hs -o bin/chapter4.exe

bin/chapter5.exe: src/chapter5.hs
  ghc src/chapter5.hs -o bin/chapter5.exe

compile: init bin/chapter1.exe bin/chapter2.exe bin/chapter3.exe bin/chapter4.exe bin/chapter5.exe

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
