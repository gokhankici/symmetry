CC      = stack ghc
CC_ARGS = -- -O2

EXES = QC TargetClient

all: $(EXES)

$(EXES): %: %.hs
	$(CC) $(CC_ARGS) $< -o $@

states: states.json

states.json: QC
	./$<

pred: TargetClient states.json
	./$<

.PHONY: clean
clean:
	rm -f *.o *.hi *.dyn_hi *.dyn_o $(EXES)
