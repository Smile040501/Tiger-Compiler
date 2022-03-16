# Binary Executable
TIG_BIN := ./tc
TIG_IMAGE :=

# Directories
TIG_DIR := tiger
TARGET_DIR := target
IR_DIR := ir
UTILS_DIR := util

# Parse Files
TIG_PARSE := $(addprefix $(TIG_DIR)/, *.grm.sml *.lex.sml)

# Source Files
SRC := $(wildcard $(TIG_DIR)/*.sig) $(wildcard $(TIG_DIR)/*.sml) \
			$(addprefix $(TIG_DIR)/, tiger.grm tiger.lex) \
			$(wildcard $(TARGET_DIR)/*.sig) $(wildcard $(TARGET_DIR)/*.sml) \
			$(wildcard $(IR_DIR)/*.sig) $(wildcard $(IR_DIR)/*.sml) \
			$(wildcard $(UTILS_DIR)/*.sig) $(wildcard $(UTILS_DIR)/*.sml) \
			tc.sml

# Test Cases
TESTS_DIR := tests
TF := $(TESTS_DIR)/custom.tig
TFF := $(basename $(TF))
TESTS_FILES := $(wildcard $(TESTS_DIR)/*.tig) $(wildcard $(TESTS_DIR)/*.out)
TEST_SCRIPT := $(TESTS_DIR)/test.sh

# Files to be cleaned
CLEANFILES := $(addprefix $(TIG_DIR)/, *.grm.sig *.grm.sml *grm.desc *lex.sml) \
				$(addprefix $(TESTS_DIR)/, *.s *.so *.o) \
					$(TIG_BIN)

# Compile Files
TIG_MLTON := tc.mlb

.PHONY: all log test tests clean

all: $(TIG_BIN)

log:
	@echo "Parse Files:"
	@echo $(TIG_PARSE)
	@echo "\nSource Files:"
	@echo $(SRC)
	@echo "\nTest Files:"
	@echo $(TESTS_FILES)
	@echo "\nClean Files:"
	@echo $(CLEANFILES)

$(TIG_BIN): $(SRC) $(TIG_PARSE) $(TIG_MLTON)
	mlton -output $@ $(TIG_MLTON)

%.lex.sml: %.lex
	mllex $<

%.grm.sml: %.grm
	mlyacc $<

run: $(TIG_BIN) $(TF)
	@./tc -D $(TF)
	@echo "--------------------------------------------------------------"
	@spim -file $(TFF).s

tc%: tests/test%.tig tests/test%.out
	@bash $(TEST_SCRIPT) $<

test: $(TIG_BIN) $(TF)
	@./tc -D $(TF)

tests: $(TIG_BIN) $(TESTS_FILES)
	@bash $(TEST_SCRIPT)

clean:
	rm -rf $(CLEANFILES)
