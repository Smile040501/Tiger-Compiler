# Binary Executable
TIG_BIN := ./tc
TIG_IMAGE :=

# Directories
TIG_DIR := tiger
TARGET_DIR := target
IR_DIR := ir

# Parse Files
TIG_PARSE := $(addprefix $(TIG_DIR)/, *.grm.sml *.lex.sml)

# Source Files
SRC := $(wildcard $(TIG_DIR)/*.sig) $(wildcard $(TIG_DIR)/*.sml) \
			$(addprefix $(TIG_DIR)/, tiger.grm tiger.lex) \
			$(wildcard $(TARGET_DIR)/*.sig) $(wildcard $(TARGET_DIR)/*.sml) \
			$(wildcard $(IR_DIR)/*.sig) $(wildcard $(IR_DIR)/*.sml) \
			tc.sml

# Files to be cleaned
CLEANFILES := $(addprefix $(TIG_DIR)/, *.grm.sig *.grm.sml *grm.desc *lex.sml) \
					$(TIG_BIN)

# Compile Files
TIG_MLTON := tc.mlb

.PHONY: all log run clean

all: $(TIG_BIN)

log:
	@echo "Parse Files:"
	@echo $(TIG_PARSE)
	@echo "\nSource Files:"
	@echo $(SRC)
	@echo "\nClean Files:"
	@echo $(CLEANFILES)

$(TIG_BIN): $(SRC) $(TIG_PARSE) $(TIG_MLTON)
	mlton -output $@ $(TIG_MLTON)

%.lex.sml: %.lex
	mllex $<

%.grm.sml: %.grm
	mlyacc $<

run: $(TIG_BIN)
	$(TIG_BIN)

clean:
	rm -rf $(CLEANFILES)
