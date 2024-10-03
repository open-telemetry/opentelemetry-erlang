defmodule OpenTelemetry.SemConv.Incubating.CloudeventsAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Cloudevents attributes.
  """

  @doc """
  The [event_id](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#id) uniquely identifies the event.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["123e4567-e89b-12d3-a456-426614174000", "0001"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CloudeventsAttributes.cloudevents_event_id()
      :"cloudevents.event_id"

  ### Erlang

  ```erlang
  ?CLOUDEVENTS_EVENT_ID.
  'cloudevents.event_id'
  ```

  <!-- tabs-close -->
  """
  @spec cloudevents_event_id :: :"cloudevents.event_id"
  def cloudevents_event_id do
    :"cloudevents.event_id"
  end

  @doc """
  The [source](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#source-1) identifies the context in which an event happened.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["https://github.com/cloudevents", "/cloudevents/spec/pull/123", "my-service"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CloudeventsAttributes.cloudevents_event_source()
      :"cloudevents.event_source"

  ### Erlang

  ```erlang
  ?CLOUDEVENTS_EVENT_SOURCE.
  'cloudevents.event_source'
  ```

  <!-- tabs-close -->
  """
  @spec cloudevents_event_source :: :"cloudevents.event_source"
  def cloudevents_event_source do
    :"cloudevents.event_source"
  end

  @doc """
  The [version of the CloudEvents specification](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#specversion) which the event uses.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  1.0
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CloudeventsAttributes.cloudevents_event_spec_version()
      :"cloudevents.event_spec_version"

  ### Erlang

  ```erlang
  ?CLOUDEVENTS_EVENT_SPEC_VERSION.
  'cloudevents.event_spec_version'
  ```

  <!-- tabs-close -->
  """
  @spec cloudevents_event_spec_version :: :"cloudevents.event_spec_version"
  def cloudevents_event_spec_version do
    :"cloudevents.event_spec_version"
  end

  @doc """
  The [subject](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#subject) of the event in the context of the event producer (identified by source).

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  mynewfile.jpg
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CloudeventsAttributes.cloudevents_event_subject()
      :"cloudevents.event_subject"

  ### Erlang

  ```erlang
  ?CLOUDEVENTS_EVENT_SUBJECT.
  'cloudevents.event_subject'
  ```

  <!-- tabs-close -->
  """
  @spec cloudevents_event_subject :: :"cloudevents.event_subject"
  def cloudevents_event_subject do
    :"cloudevents.event_subject"
  end

  @doc """
  The [event_type](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#type) contains a value describing the type of event related to the originating occurrence.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Examples

  ```
  ["com.github.pull_request.opened", "com.example.object.deleted.v2"]
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.CloudeventsAttributes.cloudevents_event_type()
      :"cloudevents.event_type"

  ### Erlang

  ```erlang
  ?CLOUDEVENTS_EVENT_TYPE.
  'cloudevents.event_type'
  ```

  <!-- tabs-close -->
  """
  @spec cloudevents_event_type :: :"cloudevents.event_type"
  def cloudevents_event_type do
    :"cloudevents.event_type"
  end
end
