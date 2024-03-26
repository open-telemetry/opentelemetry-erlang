defmodule OpenTelemetry.SemanticConventions.Event do
  @doc """
  The schema url for telemetry resources.

      iex> OpenTelemetry.SemanticConventions.Event.event_schema_url()
      "https://opentelemetry.io/schemas/1.20.0"
  """
  @spec event_schema_url :: String.t()
  def event_schema_url do
    "https://opentelemetry.io/schemas/1.20.0"
  end
  @doc """
  The unique identifier of the feature flag

      iex> OpenTelemetry.SemanticConventions.Event.feature_flag_key()
      :"feature_flag.key"
  """
  @spec feature_flag_key :: :"feature_flag.key"
  def feature_flag_key do
    :"feature_flag.key"
  end
  @doc """
  The name of the service provider that performs the flag evaluation

      iex> OpenTelemetry.SemanticConventions.Event.feature_flag_provider_name()
      :"feature_flag.provider_name"
  """
  @spec feature_flag_provider_name :: :"feature_flag.provider_name"
  def feature_flag_provider_name do
    :"feature_flag.provider_name"
  end
  @doc """
  SHOULD be a semantic identifier for a value. If one is unavailable, a stringified version of the value can be used

  ### Notes

  A semantic identifier, commonly referred to as a variant, provides a means
  for referring to a value without including the value itself. This can
  provide additional context for understanding the meaning behind a value.
  For example, the variant `red` maybe be used for the value `#c05543`.
  
  A stringified version of the value can be used in situations where a
  semantic identifier is unavailable. String representation of the value
  should be determined by the implementer

      iex> OpenTelemetry.SemanticConventions.Event.feature_flag_variant()
      :"feature_flag.variant"
  """
  @spec feature_flag_variant :: :"feature_flag.variant"
  def feature_flag_variant do
    :"feature_flag.variant"
  end
  @doc """
  Whether this is a received or sent message

      iex> OpenTelemetry.SemanticConventions.Event.message_type()
      :"message.type"
  """
  @spec message_type :: :"message.type"
  def message_type do
    :"message.type"
  end
  @doc """
  MUST be calculated as two different counters starting from `1` one for sent messages and one for received message

  ### Notes

  This way we guarantee that the values will be consistent between different implementations

      iex> OpenTelemetry.SemanticConventions.Event.message_id()
      :"message.id"
  """
  @spec message_id :: :"message.id"
  def message_id do
    :"message.id"
  end
  @doc """
  Compressed size of the message in bytes

      iex> OpenTelemetry.SemanticConventions.Event.message_compressed_size()
      :"message.compressed_size"
  """
  @spec message_compressed_size :: :"message.compressed_size"
  def message_compressed_size do
    :"message.compressed_size"
  end
  @doc """
  Uncompressed size of the message in bytes

      iex> OpenTelemetry.SemanticConventions.Event.message_uncompressed_size()
      :"message.uncompressed_size"
  """
  @spec message_uncompressed_size :: :"message.uncompressed_size"
  def message_uncompressed_size do
    :"message.uncompressed_size"
  end
  @doc """
  The type of the exception (its fully-qualified class name, if applicable). The dynamic type of the exception should be preferred over the static type in languages that support it

      iex> OpenTelemetry.SemanticConventions.Event.exception_type()
      :"exception.type"
  """
  @spec exception_type :: :"exception.type"
  def exception_type do
    :"exception.type"
  end
  @doc """
  The exception message

      iex> OpenTelemetry.SemanticConventions.Event.exception_message()
      :"exception.message"
  """
  @spec exception_message :: :"exception.message"
  def exception_message do
    :"exception.message"
  end
  @doc """
  A stacktrace as a string in the natural representation for the language runtime. The representation is to be determined and documented by each language SIG

      iex> OpenTelemetry.SemanticConventions.Event.exception_stacktrace()
      :"exception.stacktrace"
  """
  @spec exception_stacktrace :: :"exception.stacktrace"
  def exception_stacktrace do
    :"exception.stacktrace"
  end
  @doc """
  SHOULD be set to true if the exception event is recorded at a point where it is known that the exception is escaping the scope of the span

  ### Notes

  An exception is considered to have escaped (or left) the scope of a span,
  if that span is ended while the exception is still logically "in flight".
  This may be actually "in flight" in some languages (e.g. if the exception
  is passed to a Context manager's `__exit__` method in Python) but will
  usually be caught at the point of recording the exception in most languages.
  
  It is usually not possible to determine at the point where an exception is thrown
  whether it will escape the scope of a span.
  However, it is trivial to know that an exception
  will escape, if one checks for an active exception just before ending the span,
  as done in the [example above](#recording-an-exception).
  
  It follows that an exception may still escape the scope of the span
  even if the `exception.escaped` attribute was not set or set to false,
  since the event might have been recorded at a time where it was not
  clear whether the exception will escape

      iex> OpenTelemetry.SemanticConventions.Event.exception_escaped()
      :"exception.escaped"
  """
  @spec exception_escaped :: :"exception.escaped"
  def exception_escaped do
    :"exception.escaped"
  end
end