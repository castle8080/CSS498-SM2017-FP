
all: compile

bin/chapter1.exe: src/chapter1.hs
	ghc src/chapter1.hs -o bin/chapter1.exe

compile: init bin/chapter1.exe

init:
	@if [ ! -d bin ]; then mkdir bin; fi

clean:
	rm -rf src/*.o src/*.hi src/*~ bin

run: compile
	bin/chapter1.exe
