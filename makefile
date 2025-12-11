build:
	dune build pyretc.exe

clean:
	dune clean

syntax:
	clear
	dune build pyretc.exe
	./test -1 _build/default/pyretc.exe

type:
	clear
	dune build pyretc.exe
	./test -2 _build/default/pyretc.exe