all: sorting.out

sorting.out: sorting.mt preprocessor
	cat sorting.mt | ./preprocessor > sorting.out

preprocessor: preprocessor.hs
	ghc preprocessor.hs

clean:
	$(RM) preprocessor.hi preprocessor.o preprocessor sorting.out
