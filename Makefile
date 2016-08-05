.PHONY: all clean
INSTALL_ROOT:=$(shell stack path --local-install-root)
CLOSURE:=java -jar ~/Software/closure/compiler.jar

all: js-build/install-root js-build/app.min.js

js-build/install-root: $(INSTALL_ROOT)
	mkdir -p js-build
	rm -f js-build/install-root
	ln -s $(INSTALL_ROOT) js-build/install-root

js-build/queue-animations.min.js: js-build/queue-animations.js
	$(CLOSURE) --compilation_level=ADVANCED_OPTIMIZATIONS js-build/queue-animations.js > js-build/queue-animations.min.js

js-build/app.min.js: js-build/queue-animations.min.js
	cat deps/react.min.js deps/react-dom.min.js js-build/queue-animations.min.js > js-build/app.min.js

js-build/queue-animations.js: $(INSTALL_ROOT)/bin/queue-animations-output.jsexe/all.js
	mkdir -p js-build
	echo "(function(global,React,ReactDOM) {" > js-build/queue-animations.js
	cat $(INSTALL_ROOT)/bin/queue-animations-output.jsexe/all.js | sed 's/goog.provide.*//' | sed 's/goog.require.*//' >> js-build/queue-animations.js
	echo "})(window, window['React'], window['ReactDOM']);" >> js-build/queue-animations.js

clean:
	rm -rf js-build
