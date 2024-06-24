defmodule OpenTelemetry.SemanticConventions.CloudeventsAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Cloudevents attributes.
  """

  @doc """
  The [event_id](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#id) uniquely identifies the event.



  ### Example
      iex> OpenTelemetry.SemanticConventions.CloudeventsAttributes.cloudevents_eventid()
      :"cloudevents.event_id"
  """
  @spec cloudevents_eventid :: :"cloudevents.event_id"
  def cloudevents_eventid do
    :"cloudevents.event_id"
  end

  @doc """
  The [source](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#source-1) identifies the context in which an event happened.



  ### Example
      iex> OpenTelemetry.SemanticConventions.CloudeventsAttributes.cloudevents_eventsource()
      :"cloudevents.event_source"
  """
  @spec cloudevents_eventsource :: :"cloudevents.event_source"
  def cloudevents_eventsource do
    :"cloudevents.event_source"
  end

  @doc """
  The [version of the CloudEvents specification](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#specversion) which the event uses.



  ### Example
      iex> OpenTelemetry.SemanticConventions.CloudeventsAttributes.cloudevents_eventspecversion()
      :"cloudevents.event_spec_version"
  """
  @spec cloudevents_eventspecversion :: :"cloudevents.event_spec_version"
  def cloudevents_eventspecversion do
    :"cloudevents.event_spec_version"
  end

  @doc """
  The [subject](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#subject) of the event in the context of the event producer (identified by source).



  ### Example
      iex> OpenTelemetry.SemanticConventions.CloudeventsAttributes.cloudevents_eventsubject()
      :"cloudevents.event_subject"
  """
  @spec cloudevents_eventsubject :: :"cloudevents.event_subject"
  def cloudevents_eventsubject do
    :"cloudevents.event_subject"
  end

  @doc """
  The [event_type](https://github.com/cloudevents/spec/blob/v1.0.2/cloudevents/spec.md#type) contains a value describing the type of event related to the originating occurrence.



  ### Example
      iex> OpenTelemetry.SemanticConventions.CloudeventsAttributes.cloudevents_eventtype()
      :"cloudevents.event_type"
  """
  @spec cloudevents_eventtype :: :"cloudevents.event_type"
  def cloudevents_eventtype do
    :"cloudevents.event_type"
  end
end
