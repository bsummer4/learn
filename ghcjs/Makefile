hello.jsexe/all.js: hello.hs
	ghcjs -DGHCJS_BROWSER $^

min.js.gz: min.js
	gzip --best <$^ >$@

min.js: hello.jsexe/all.js
	echo "(function(global) {" >tmp.js
	cat hello.jsexe/all.js >>tmp.js
	echo "})(typeof global !== 'undefined' ? global : this);" >>tmp.js
	closure-compiler tmp.js --compilation_level=ADVANCED_OPTIMIZATIONS >$@
	rm -f tmp.js

clean:
	rm -rf hello.jsexe
	rm -f *.js_hi *.js_o min.js min.js.gz

test: min.js min.js.gz
	du -sh min.js.gz
	node min.js
