ifeq ($(OS),Windows_NT)
	PROGNAME = main.exe
else 
	PROGNAME = main
endif

test: main face.mdl src/py/main.py src/py/mdl.py src/py/script.py
	./main face.mdl

clean:
	-rm *pyc *out parsetab.py
	-rm src/*pyc src/*out src/parsetab.py
	-rm src/pas/*.o src/pas/*.ppu 

main: src/pas/main.pas src/pas/lines.pas src/pas/graphicsmatrix.pas src/pas/curves.pas src/pas/shapes.pas src/pas/objreader.pas src/pas/gmath.pas src/pas/matrixstack.pas src/pas/stringhelper.pas
	fpc src/pas/main.pas -Px86_64 -O3
	cp src/pas/$(PROGNAME) ./

objreader: src/pas/objreader.pas src/pas/lines.pas src/pas/graphicsmatrix.pas src/pas/curves.pas src/pas/shapes.pas src/pas/gmath.pas src/pas/matrixstack.pas src/pas/stringhelper.pas
	fpc src/pas/objreader.pas -Px86_64 -O3

clear:
	-rm *pyc *out src/py/parsetab.py 
	-rm *.ppm *.png
