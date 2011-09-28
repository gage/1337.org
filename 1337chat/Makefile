DEPS = gproc jsonerl erlmongo cowboy gen_smtp erlydtl

all: deps
	./rebar compile

run: all
	ERL_LIBS=deps erl -pa apps/*/ebin -config dev -s ltchat_app

clean:
	./rebar clean


deps: $(patsubst %, deps/%, $(DEPS))

deps/%:
	./rebar get-deps
	cd deps && agner build $*

depclean:
	rm -Rf deps/*

release:
	rm -Rf rel/glchatnode_bak
	mv rel/glchatnode rel/glchatnode_bak
	./rebar generate

appup:
	./rebar generate-appups previous_release=glchatnode_bak

startrel:
	./rel/glchatnode/bin/glchatnode start

stoprel:
	./rel/glchatnode/bin/glchatnode stop

test: all
	ERL_LIBS=deps erl -pa apps/*/ebin -config dev -boot start_sasl -noshell -s glchat_hunger test -s init stop
