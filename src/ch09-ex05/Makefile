all: calc life

clean:
	rm calc.hi calc.o calc
	rm life.hi life.o life

calc: calc.hs
	ghc --make calc.hs -o calc

life: life.hs
	ghc --make life.hs -o life
