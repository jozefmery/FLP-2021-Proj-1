#	File:    Makefile
# 	Project: FLP-2021-xmeryj00-simplify-bkg
#	Author:  Jozef Méry - xmeryj00@vutbr.cz
#	Date:	 9.2.2021


# $@ - target
# $< - first dep
# $^ - all deps

# archive properties
ARCHIVE 	= flp-fun-xmeryj00
ARCHIVEEXT  = zip
ARCHIVER 	= zip

# helper programs
DIRMAKER 	= @mkdir -p

# directory definitions
BINDIR 		= bin
SRCDIR		= src
OBJDIR 		= build

# target name
TARGET      = simplify-bkg
DEBUGTARGET = dsimplify-bkg

# archive content definition
ARCHIVELIST = $(SRCDIR)/ Makefile doc/

# file extensions
SRCEXT 		= hs

# compiler options 
CC          = ghc
CFLAGS      = -Wall --make
RELCFLAGS	= -O2
DCFLAGS		= -g -O0

# link libraries
LIBS		= $(addprefix -l, )
LIBDIRS		= $(addprefix -L, )

default: release
.PHONY: default clean run archive crun debug release

# object directory structure
RELDIR	= Release
DDIR	= Debug

# fetch sources
SOURCES  = $(wildcard $(SRCDIR)/*.$(SRCEXT))

# object directory structure targets
$(OBJDIR):
	$(DIRMAKER) $(OBJDIR)

$(OBJDIR)/$(DDIR): $(OBJDIR)
	$(DIRMAKER) $(OBJDIR)/$(DDIR)

$(OBJDIR)/$(RELDIR): $(OBJDIR)
	$(DIRMAKER) $(OBJDIR)/$(RELDIR)

# binary directory target
$(BINDIR):
	$(DIRMAKER) $(BINDIR)

# compile in release mode
$(BINDIR)/$(TARGET): $(SOURCES) | $(BINDIR) $(OBJDIR)/$(RELDIR)
	$(CC) $(CFLAGS) $(RELCFLAGS) $(LIBS) $(LIBDIRS) src/Main.hs -o $@ -outputdir $(OBJDIR)/$(RELDIR)/ -isrc

# compile in debug mode
$(BINDIR)/$(DEBUGTARGET): $(SOURCES) | $(BINDIR) $(OBJDIR)/$(DDIR)
	$(CC) $(CFLAGS) $(DCFLAGS) $(LIBS) $(LIBDIRS) src/Main.hs -o $@ -outputdir $(OBJDIR)/$(DDIR)/ -isrc

release: $(BINDIR)/$(TARGET)
debug: $(BINDIR)/$(DEBUGTARGET)

# run
run: release
	@./$(BINDIR)/$(TARGET) $(ARGS)

# run with clear
crun: release
	@clear
	@./$(BINDIR)/$(TARGET) $(ARGS)

# clean directory
clean:
	-rm -rf $(OBJDIR)/
	-rm -rf $(BINDIR)/
	-rm -f  $(ARCHIVE).$(ARCHIVEEXT)

# create final archive
archive: $(ARCHIVE).$(ARCHIVEEXT)

$(ARCHIVE).$(ARCHIVEEXT): $(ARCHIVELIST)
	$(ARCHIVER) $@ $^