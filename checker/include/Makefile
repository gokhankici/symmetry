# To build with profiling support:
# $> stack build --library-profiling --executable-profiling --ghc-options="-fprof-auto -rtsopts"

CC           = stack exec -- ghc
CC_ARGS      = -O2 -rtsopts
CC_ARGS_PROF = -prof -fprof-auto
MAX_MEM      = 3G

STATE_EXE  = QC
TARGET_EXE = TargetClient
EXES       = $(STATE_EXE) $(TARGET_EXE)

STATES         = states.json
PREDCOUNT      = 100000
PREDICATES     = predicates.json
PREDICATES_LOG = predicates.log

REMOTE_FOLDER_NAME = /home/rkici/symmetry
REMOTE_ADDR        = goto:$(REMOTE_FOLDER_NAME)

all: $(EXES)

$(EXES): %: %.hs
	$(CC) --make $< $(CC_ARGS)

# Generate sample states and infer invariants ###################################

states: $(STATES)

$(STATES): $(STATE_EXE)
	./$<

pred: $(TARGET_EXE)
	./$< --pred-count $(PREDCOUNT) \
	     +RTS -M$(MAX_MEM) -RTS \
	     2>&1 1>$(PREDICATES_LOG)
	cat $(PREDICATES)

# Run the invariant generator on goto ###########################################

define REMOTE_SCRIPT
cd $(REMOTE_FOLDER_NAME) &&    \
./$< --pred-count $(PREDCOUNT) \
     +RTS -M$(MAX_MEM) -RTS    \
     2>&1 1>$(PREDICATES_LOG)
endef

run-on-goto: $(TARGET_EXE)
	test -e $(STATES)
	rsync -azP $(STATES)     $(REMOTE_ADDR)/$(STATES)
	rsync -azP $(TARGET_EXE) $(REMOTE_ADDR)/$(TARGET_EXE)
	ssh goto "$(REMOTE_SCRIPT)"
	scp $(REMOTE_ADDR)/$(PREDICATES_LOG) ./
	scp $(REMOTE_ADDR)/$(PREDICATES)     ./


# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------


# Profile predicate generation code #############################################

profile: $(TARGET_EXE).hp

$(TARGET_EXE).hp:
	rm -f $(TARGET_EXE)
	$(CC) --make $(TARGET_EXE) $(CC_ARGS) $(CC_ARGS_PROF)
	./$(TARGET_EXE) +RTS -p -hc -M$(MAX_MEM) -RTS

# Plot heap statistics ##########################################################

print-profile: $(TARGET_EXE).svg

$(TARGET_EXE).svg: $(TARGET_EXE).hp
	hp2pretty $<
	xdg-open $(TARGET_EXE).svg 2>&1 1>/dev/null &


# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------


.PHONY: clean pred states profile print-profile

clean:
	rm -f $(EXES) *.o *.hi *.dyn_hi *.dyn_o \
          *.aux *.hp *.pdf *.prof *.ps
