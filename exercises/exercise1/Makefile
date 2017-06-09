
all: compile

exercise1.exe: exercise1.hs
	ghc *.hs -o exercise1.exe

compile: exercise1.exe

clean:
	rm -rf *.o *.hi *~ *.exe

run: compile
	./exercise1.exe
