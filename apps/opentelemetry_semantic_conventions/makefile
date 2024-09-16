SEMCON_VERSION = 1.27.0

.PHONY: setup clean-all checkout generate

clean-all: clean-semconv clean-elixir clean-erlang

clean-semconv:
	rm -rf semtmp
	rm -rf guides/

clean-elixir:
	rm -rf lib/attributes
	rm -rf lib/incubating
	rm -rf lib/metrics
	rm -rf doc

clean-erlang:
	rm -rf include/attributes
	rm -rf include/incubating
	rm -rf include/metrics

setup:
	curl --proto '=https' --tlsv1.2 -LsSf https://github.com/open-telemetry/weaver/releases/download/v0.9.2/weaver-installer.sh | sh

checkout:
	mkdir semtmp
	git clone https://github.com/open-telemetry/semantic-conventions.git semtmp
	cd semtmp; \
	  git checkout v$(SEMCON_VERSION) && \
	  cp -r docs ../guides
	rm -rf guides/docs
	rm -f $(addprefix guides/attributes-registry/, android.md aspnetcore.md dotnet.md go.md ios.md jvm.md kestrel.md nodejs.md signalr.md v8js.md veightjs.md webengine.md)

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