`otel-sdk-config.json` is the upstream reference configuration converted to JSON
without changing any values, using `json.dumps(yaml.safe_load(source), indent=2)`.

Source: https://github.com/open-telemetry/opentelemetry-configuration/blob/fce52c3f13cc96f41ee5493598d1354a62367641/examples/otel-sdk-config.yaml

The YAML conversion is performed when updating this fixture, not during tests.
