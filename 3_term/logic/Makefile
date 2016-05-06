#  ghc -outputdirbuild --make -O3 Deduction.hs -prof -auto-all -caf-all -fforce-recomp -o bin/Deduction1

CC=ghc 
OPTIONS=-outputdirbuild -O3 --make

ifdef prof
	OPTIONS+= -prof -auto-all -caf-all
endif

#ifdef re
OPTIONS+= -fforce-recomp
#endif

all:
	mkdir -p build bin
	$(CC) $(OPTIONS) Statement/Annotate.hs -o bin/Annotate
	$(CC) $(OPTIONS) Statement/Deduction.hs -o bin/Deduction
	$(CC) $(OPTIONS) Statement/Proove.hs -o bin/Proove
	$(CC) $(OPTIONS) Predicate/Predicate.hs -o bin/Predicate
	$(CC) $(OPTIONS) Predicate/AplusB.hs -o bin/AplusB
