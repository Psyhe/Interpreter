HC = ghc
GHC = ghc

# Compiler flags
HCFLAGS = -Wno-missing-methods
# The target executable
TARGET_DIR = examples
TARGET = $(TARGET_DIR)/interpreter

# BNFC command and options
BNFC = /home/students/inf/PUBLIC/MRJP/bin/bnfc
BNFC_OPTS = --outputdir=source --functor source/wyso.cf

# Happy and Alex commands and options
HAPPY = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX = alex
ALEX_OPTS = --ghc

# Source files
SRCS = source/AbsWyso.hs source/EvaluatorEnv.hs source/EvaluatorTypes.hs source/Interpreter.hs source/PrintWyso.hs \
       source/TypecheckerTypes.hs source/Evaluator.hs source/Exceptions.hs source/ParWyso.hs source/LexWyso.hs \
       source/Main.hs source/SkelWyso.hs source/TypecheckerEnv.hs source/Typechecker.hs

# List of goals not corresponding to file names.
.PHONY: all clean distclean

# Default goal
all: $(TARGET)

# Ensure the target directory exists
$(TARGET_DIR):
	mkdir -p $(TARGET_DIR)

# Rule to generate parser files
source/AbsWyso.hs source/LexWyso.x source/ParWyso.y source/PrintWyso.hs: source/wyso.cf
	$(BNFC) $(BNFC_OPTS)

# Rules for building the parser
source/%.hs: source/%.y
	$(HAPPY) $(HAPPY_OPTS) -o $@ $<

source/%.hs: source/%.x
	$(ALEX) $(ALEX_OPTS) -o $@ $<

# Rule to link object files to create the executable
$(TARGET): $(TARGET_DIR) $(SRCS)
	$(HC) $(HCFLAGS) -o $(TARGET) $(SRCS)

# Clean up object files and the executable
clean:
	rm -f *.hi *.o $(TARGET) source/*.hi source/*.o source/*.log source/*.aux source/*.dvi

distclean: clean
	rm -f source/AbsWyso.hs source/AbsWyso.hs.bak source/ComposOp.hs source/ComposOp.hs.bak source/DocWyso.txt source/DocWyso.txt.bak \
	       source/ErrM.hs source/ErrM.hs.bak source/LayoutWyso.hs source/LayoutWyso.hs.bak source/LexWyso.x source/LexWyso.x.bak \
	       source/ParWyso.y source/ParWyso.y.bak source/PrintWyso.hs source/PrintWyso.hs.bak source/SkelWyso.hs source/SkelWyso.hs.bak \
	       source/TestWyso.hs source/TestWyso.hs.bak source/XMLWyso.hs source/XMLWyso.hs.bak source/ASTWyso.agda source/ASTWyso.agda.bak \
	       source/ParserWyso.agda source/ParserWyso.agda.bak source/IOLib.agda source/IOLib.agda.bak source/Main.agda source/Main.agda.bak \
	       source/wyso.dtd source/wyso.dtd.bak source/LexWyso.hs source/ParWyso.hs source/ParWyso.info source/ParDataWyso.hs source/Makefile