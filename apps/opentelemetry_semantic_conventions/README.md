# Opentelemetry Semantic Conventions

Auto-generated constants that represent the [OpenTelemetry Semantic
Conventions](https://github.com/open-telemetry/semantic-conventions).

## Usage

Comprehensive documentation is available on [hexdocs](https://hexdocs.pm/opentelemetry_semantic_conventions) for Elixir and Erlang.

Check out the Guides tab on HexDocs for detailed attribute information
and usage.

### Migration from v1.13.0 (v0.2.0 package version) semantic conventions

The structure of OpenTelemetry Semantic Conventions has evolved a great
deal since the last version we have published. All attributes now live
under a common attribute registry. In addition, attributes have been classified
as stable or experimental.

Attributes are now organized by attribute group and stability. The prior code
has been kept in a deprecated status to allow backward compatability
during migration.

### Erlang

For Erlang include the semantic conventions header for the particular kind you
need:

```
-include_lib("opentelemetry_semantic_conventions/include/attributes/url_attributes.hrl").
```

You can then use the macros for the attribute keys:

```
?URL_PATH
```

#### Enums

Enum Attribute types define each value in a macro with the attribute name prefixed.

Enum Attributes allow for a user-supplied value when no pre-defined option exists. Users
may set this value manually while paying attention to the required value type

#### Incubating Attributes & Metrics

Incubating attribute header files are located in the `incubating` folder and metrics under `incubating/metrics`.

Experimental attributes are considered to be incubating. Attribute groups can
contain attributes which are stable, experimental, or both. Experimental
attributes are contained in an incubating header. Attribute groups containing
attributes of both stability levels will have two header files in this case.

```
-include_lib("opentelemetry_semantic_conventions/include/incubating/attributes/url_attributes.hrl").
```

You can then use the macros for the experimental attribute keys:

```
?URL_DOMAIN
```

### Elixir

Attributes in Elixir are defined as functions. To use an attribute, simply call it
with that attribute's name.

```
iex> OpenTelemetry.SemConv.URLAttributes.url_path()
:"url.path"
```

#### Enums

Enum Attributes allow for a user-supplied value when no pre-defined option exists. Users
may set this value manually while paying attention to the required value type

```
iex> OpenTelemetry.SemConv.Incubating.DBAttributes.db_system_values().postgresql
:postgresql
```

Enum Attributes allow for a user-supplied value when no pre-defined option exists. Users
may set this value manually while paying attention to the required value type

#### Incubating Attributes

Incubating attributes are located under the `OpenTelemetry.SemConv.Incubating`
namespace.

Experimental attributes are considered to be incubating. Attribute groups can
contain attributes which are stable, experimental, or both. Experimental
attributes are contained in an incubating module. Attribute groups containing
attributes of both stability levels will have two modules in this case.

## Contribution

The semantic conventions are auto-generated from the definitions in
[OpenTelemetry Semantic
Conventions](https://github.com/open-telemetry/semantic-conventions) repository.
To generate the file, execute the generator by invoking `elixir generate.exs` in the
root of this folder. The definitions version to be generated is set in the `generate.exs`
file.
