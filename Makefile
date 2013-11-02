#
# Project: SHWS - Simple Haskell Web Server
# Author:  Petr Zemek, s3rvac@gmail.com, 2009
#
# Usage:
#   - compile project:       make
#   - make tester:           make test
#   - create documentation:  make docs
#   - clean:                 make clean
#

# Compiler
HC = ghc
HCFLAGS = -Wall -O2 -threaded --make

# Output paths
OUTDIR = .
OUTPROJFILE = $(OUTDIR)/shws
OUTTESTFILE = $(OUTDIR)/tester

# Documentation paths
DOCSDIR = ./doc
DOCSURL = http://haskell.org/ghc/docs/latest/html/libraries
HDIDIR = /usr/share/doc/ghc6-doc/libraries

# Source paths
SRCDIR = ./src
SRCFILES = $(SRCDIR)/Main.hs \
	$(SRCDIR)/Common.hs \
	$(SRCDIR)/ConfigParser.hs \
	$(SRCDIR)/Log.hs \
	$(SRCDIR)/Mime.hs \
	$(SRCDIR)/RequestHandler.hs \
	$(SRCDIR)/Server.hs \
	$(SRCDIR)/ServerConfig.hs
TESTSRCFILES = $(SRCDIR)/Test/Main.hs \
	$(SRCDIR)/Test/CommonTest.hs \
	$(SRCDIR)/Test/ConfigParserTest.hs \
	$(SRCDIR)/Test/LogTest.hs \
	$(SRCDIR)/Test/MimeTest.hs \
	$(SRCDIR)/Test/RequestHandlerTest.hs \
	$(SRCDIR)/Test/ServerTest.hs \
	$(SRCDIR)/Test/ServerConfigTest.hs

# Project compilation
all: $(SRCFILES)
	$(HC) $(HCFLAGS) $(SRCFILES) -o $(OUTPROJFILE)

# Make tester
test: $(SRCFILES) $(TESTSRCFILES)
	$(HC) $(HCFLAGS) $(TESTSRCFILES) -i$(SRCDIR) -o $(OUTTESTFILE)

# Documentation
docs:
	haddock -v -h -t "SHWS - Simple Haskell Web Server" \
		--ignore-all-exports \
		-i $(DOCSURL)/array,$(HDIDIR)/array/array.haddock \
		-i $(DOCSURL)/base,$(HDIDIR)/base/base.haddock \
		-i $(DOCSURL)/bytestring,$(HDIDIR)/bytestring/bytestring.haddock \
		-i $(DOCSURL)/containers,$(HDIDIR)/containers/containers.haddock \
		-i $(DOCSURL)/directory,$(HDIDIR)/directory/directory.haddock \
		-i $(DOCSURL)/filepath,$(HDIDIR)/filepath/filepath.haddock \
		-i $(DOCSURL)/haskell98,$(HDIDIR)/haskell98/haskell98.haddock \
		-i $(DOCSURL)/hpc,$(HDIDIR)/hpc/hpc.haddock \
		-i $(DOCSURL)/html,$(HDIDIR)/html/html.haddock \
		-i $(DOCSURL)/hunit,$(HDIDIR)/hunit/HUnit.haddock \
		-i $(DOCSURL)/mtl,$(HDIDIR)/mtl/mtl.haddock \
		-i $(DOCSURL)/network,$(HDIDIR)/network/network.haddock \
		-i $(DOCSURL)/old-locale,$(HDIDIR)/old-locale/old-locale.haddock \
		-i $(DOCSURL)/old-time,$(HDIDIR)/old-time/old-time.haddock \
		-i $(DOCSURL)/parallel,$(HDIDIR)/parallel/parallel.haddock \
		-i $(DOCSURL)/pretty,$(HDIDIR)/pretty/pretty.haddock \
		-i $(DOCSURL)/process,$(HDIDIR)/process/process.haddock \
		-i $(DOCSURL)/random,$(HDIDIR)/random/random.haddock \
		-i $(DOCSURL)/readline,$(HDIDIR)/readline/readline.haddock \
		-i $(DOCSURL)/stream,$(HDIDIR)/stream/Stream.haddock \
		-i $(DOCSURL)/template-haskell,$(HDIDIR)/template-haskell/template-haskell.haddock \
		-i $(DOCSURL)/time,$(HDIDIR)/time/time.haddock \
		-i $(DOCSURL)/unix,$(HDIDIR)/unix/unix.haddock \
		-o $(DOCSDIR) $(SRCFILES) $(TESTSRCFILES)

# Cleaning
clean:
	rm -rf $(SRCDIR)/*.o $(SRCDIR)/*.hi $(SRCDIR)/Test/*.o $(SRCDIR)/Test/*.hi
	rm -f $(OUTPROJFILE) $(OUTTESTFILE)
	rm -f $(DOCSDIR)/*
