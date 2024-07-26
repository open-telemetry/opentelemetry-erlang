SEMCON_VERSION = 1.26.0

.PHONY: setup clean checkout generate

clean:
	rm -rf semtmp
	rm -rf guides/

setup:
	curl --proto '=https' --tlsv1.2 -LsSf https://github.com/open-telemetry/weaver/releases/download/v0.7.0/weaver-installer.sh | sh

checkout:
	mkdir semtmp
	git clone https://github.com/open-telemetry/semantic-conventions.git semtmp
	cd semtmp; \
	  git checkout v$(SEMCON_VERSION) && \
	  cp -r docs ../guides
	rm -rf guides/docs

generate: generate-elixir generate-erlang format-and-test

generate-elixir:
	weaver registry generate --registry=semtmp/model --templates=templates --param output=lib/ --param stability=stable elixir .
	weaver registry generate --registry=semtmp/model --templates=templates --param output=lib/incubating/ --param stability=experimental elixir .

generate-erlang:
	weaver registry generate --registry=semtmp/model --templates=templates --param output=include/ --param stability=stable erlang .
	weaver registry generate --registry=semtmp/model --templates=templates --param output=include/incubating/ --param stability=experimental erlang .

format-and-test:
	mix format
	mix docs
	mix test