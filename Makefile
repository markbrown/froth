# Froth Makefile

MMC = mmc
SRCDIR = src
BINDIR = bin

# Main targets
.PHONY: all clean test froth lexer_test parser_test eval_test

all: froth

froth:
	cd $(SRCDIR) && $(MMC) --make froth
	cp $(SRCDIR)/froth $(BINDIR)/

lexer_test:
	cd $(SRCDIR) && $(MMC) --make lexer_test
	cp $(SRCDIR)/lexer_test $(BINDIR)/

parser_test:
	cd $(SRCDIR) && $(MMC) --make parser_test
	cp $(SRCDIR)/parser_test $(BINDIR)/

eval_test:
	cd $(SRCDIR) && $(MMC) --make eval_test
	cp $(SRCDIR)/eval_test $(BINDIR)/

test: froth
	./run_tests.sh

clean:
	rm -rf $(SRCDIR)/Mercury
	rm -f $(SRCDIR)/froth $(SRCDIR)/lexer_test $(SRCDIR)/parser_test $(SRCDIR)/eval_test
	rm -f $(SRCDIR)/*.err
	rm -f $(BINDIR)/froth $(BINDIR)/lexer_test $(BINDIR)/parser_test $(BINDIR)/eval_test
