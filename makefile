build:
	dune build pyretc.exe

clean:
	dune clean

ex:
	clear
	dune build pyretc.exe
	./test -1 _build/default/pyretc.exe