
all: compile

clean:
	rm -f *.o *.exe *.hi *~

run: compile
	./boggle.exe

compile: boggle.exe

boggle.exe: main.hs WordSet.hs
	ghc -O2 -o boggle.exe main.hs WordSet.hs
