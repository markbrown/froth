# Froth Makefile

MMC = mmc
SRCDIR = src
TESTDIR = src/unit_tests
BINDIR = bin

# Main targets
.PHONY: all clean test froth lexer_test parser_test eval_test stack_test

all: froth

froth:
	cd $(SRCDIR) && $(MMC) --make froth
	cp $(SRCDIR)/froth $(BINDIR)/

lexer_test:
	cd $(SRCDIR) && ln -sf unit_tests/lexer_test.m && $(MMC) --make lexer_test && rm lexer_test.m
	cp $(SRCDIR)/lexer_test $(BINDIR)/

parser_test:
	cd $(SRCDIR) && ln -sf unit_tests/parser_test.m && $(MMC) --make parser_test && rm parser_test.m
	cp $(SRCDIR)/parser_test $(BINDIR)/

eval_test:
	cd $(SRCDIR) && ln -sf unit_tests/eval_test.m && $(MMC) --make eval_test && rm eval_test.m
	cp $(SRCDIR)/eval_test $(BINDIR)/

stack_test:
	cd $(SRCDIR) && ln -sf unit_tests/stack_test.m && $(MMC) --make stack_test && rm stack_test.m
	cp $(SRCDIR)/stack_test $(BINDIR)/

test: froth
	./run_tests.sh

clean:
	rm -rf $(SRCDIR)/Mercury
	rm -f $(SRCDIR)/froth $(SRCDIR)/lexer_test $(SRCDIR)/parser_test $(SRCDIR)/eval_test $(SRCDIR)/stack_test
	rm -f $(SRCDIR)/*.err $(TESTDIR)/*.err
	rm -f $(BINDIR)/froth $(BINDIR)/lexer_test $(BINDIR)/parser_test $(BINDIR)/eval_test $(BINDIR)/stack_test
