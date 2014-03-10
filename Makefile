NAME = Yolk

SOURCES = \
	src/error.ml \
	src/terms.ml \
	src/modules.ml \
	src/libraries.ml \
	src/commands.ml4 \
	src/yolk_plugin.mllib \
	src/Yolk.v

PLUGIN_MAKEFILE = Makefile.plugin

COQTOP = coqtop
COQ_MAKEFILE = coq_makefile

.PHONY: plugin install test clean

plugin: $(PLUGIN_MAKEFILE)
	$(MAKE) -f $(PLUGIN_MAKEFILE)

install: $(PLUGIN_MAKEFILE)
	$(MAKE) -f $(PLUGIN_MAKEFILE) install

test: plugin
	$(COQTOP) -batch -R src $(NAME) -I test -l Test

clean: $(PLUGIN_MAKEFILE)
	$(MAKE) -f $(PLUGIN_MAKEFILE) clean
	rm -f $(PLUGIN_MAKEFILE)
	rm -f test/*.ylk

# Use coq_makefile to generate a Makefile for the plugin.

$(PLUGIN_MAKEFILE): Makefile $(SOURCES)
	$(COQ_MAKEFILE) -I src -R src $(NAME) $(SOURCES) -o $(PLUGIN_MAKEFILE)

