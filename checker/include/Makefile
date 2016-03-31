CC      = stack exec -- ghc
CC_ARGS = -prof -fprof-auto -O2

EXES         = QC TargetClient
PROFILE_EXES = $(patsubst %,profile-%,$(EXES))

all: $(EXES)

$(EXES): %: %.hs
	$(CC) --make $< $(CC_ARGS)

states: states.json

states.json: QC
	./$<

pred: TargetClient states.json
	./$<

.PHONY: clean
clean:
	rm -f *.o *.hi *.dyn_hi *.dyn_o $(EXES) *.aux *.hp *.pdf *.prof *.ps

$(PROFILE_EXES): profile-%: %
	stack exec -- ./$< +RTS -p -hc

print-profile: TargetClient.hp
	hp2pretty $<
	open TargetClient.svg
