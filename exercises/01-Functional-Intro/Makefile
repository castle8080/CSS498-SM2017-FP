
all: compile

chapter1.exe: chapter1.hs
	ghc *.hs -o chapter1.exe

compile: chapter1.exe

clean:
	rm -rf *.o *.hi *~ *.exe

run: compile
	./chapter1.exe
