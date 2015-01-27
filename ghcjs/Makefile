hello.jsexe: hello.hs
	ghcjs $^

clean:
	rm -rf hello.jsexe
	rm -f *.js_hi *.js_o

test: hello.jsexe
	node hello.jsexe/all.js