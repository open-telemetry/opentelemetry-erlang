defmodule OpenTelemetry.SemConv.Incubating.ProfileAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Profile attributes.
  """

  @typedoc """
  Describes the interpreter or compiler of a single frame.


  ### Enum Values
  * `:dotnet` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [.NET](https://wikipedia.org/wiki/.NET)

  * `:jvm` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [JVM](https://wikipedia.org/wiki/Java_virtual_machine)

  * `:kernel` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Kernel](https://wikipedia.org/wiki/Kernel_(operating_system))

  * `:native` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - Can be one of but not limited to [C](https://wikipedia.org/wiki/C_(programming_language)), [C++](https://wikipedia.org/wiki/C%2B%2B), [Go](https://wikipedia.org/wiki/Go_(programming_language)) or [Rust](https://wikipedia.org/wiki/Rust_(programming_language)). If possible, a more precise value MUST be used.

  * `:perl` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Perl](https://wikipedia.org/wiki/Perl)

  * `:php` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [PHP](https://wikipedia.org/wiki/PHP)

  * `:cpython` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Python](https://wikipedia.org/wiki/Python_(programming_language))

  * `:ruby` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Ruby](https://wikipedia.org/wiki/Ruby_(programming_language))

  * `:v8js` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [V8JS](https://wikipedia.org/wiki/V8_(JavaScript_engine))

  * `:beam` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Erlang](https://en.wikipedia.org/wiki/BEAM_(Erlang_virtual_machine))

  * `:go` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Go](https://wikipedia.org/wiki/Go_(programming_language)),

  * `:rust` ^[e](`m:OpenTelemetry.SemConv#experimental`)^ - [Rust](https://wikipedia.org/wiki/Rust_(programming_language))

  """
  @type profile_frame_type_values() :: %{
          :dotnet => :dotnet,
          :jvm => :jvm,
          :kernel => :kernel,
          :native => :native,
          :perl => :perl,
          :php => :php,
          :cpython => :cpython,
          :ruby => :ruby,
          :v8js => :v8js,
          :beam => :beam,
          :go => :go,
          :rust => :rust
        }
  @doc """
  Describes the interpreter or compiler of a single frame.


  ### Examples

  ```
  ["cpython"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ProfileAttributes.profile_frame_type()
      :"profile.frame.type"

      iex> OpenTelemetry.SemConv.Incubating.ProfileAttributes.profile_frame_type_values().dotnet
      :dotnet

      iex> %{OpenTelemetry.SemConv.Incubating.ProfileAttributes.profile_frame_type() => OpenTelemetry.SemConv.Incubating.ProfileAttributes.profile_frame_type_values().dotnet}
      %{:"profile.frame.type" => :dotnet}

  ### Erlang

  ```erlang
  ?PROFILE_FRAME_TYPE.
  'profile.frame.type'

  ?PROFILE_FRAME_TYPE_VALUES_DOTNET.
  'dotnet'

  \#{?PROFILE_FRAME_TYPE => ?PROFILE_FRAME_TYPE_VALUES_DOTNET}.
  \#{'profile.frame.type' => 'dotnet'}
  ```

  <!-- tabs-close -->
  """
  @spec profile_frame_type :: :"profile.frame.type"
  def profile_frame_type do
    :"profile.frame.type"
  end

  @spec profile_frame_type_values() :: profile_frame_type_values()
  def profile_frame_type_values() do
    %{
      :dotnet => :dotnet,
      :jvm => :jvm,
      :kernel => :kernel,
      :native => :native,
      :perl => :perl,
      :php => :php,
      :cpython => :cpython,
      :ruby => :ruby,
      :v8js => :v8js,
      :beam => :beam,
      :go => :go,
      :rust => :rust
    }
  end
end
