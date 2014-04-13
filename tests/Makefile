all:
	ghc -Onot -fasm Unit.hs --make && ./Unit
#	runhaskell Unit.hs && echo $$?

test::
	ghc -ddump-simpl-stats -lpcre -fhpc --make -i.. -i../dist/build/ Unit.hs -o Unit -O2 -no-recomp
	rm -f *.tix
	./Unit 
	hpc report --decl-list       --exclude=Main Unit
	hpc markup --fun-entry-count --exclude=Main Unit

