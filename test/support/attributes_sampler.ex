defmodule AttributesSampler do
  def setup(attributes) when is_map(attributes) do
    attributes
  end

  def setup(_) do
    %{}
  end

  def description(_) do
    "AttributesSampler"
  end

  def should_sample(
        _ctx,
        _trace_id,
        _links,
        _span_name,
        _span_kind,
        attributes,
        config_attributes
      ) do
    case :maps.intersect(attributes, config_attributes) do
      map when map_size(map) > 0 ->
        {:drop, [], []}

      _ ->
        {:record_and_sample, [], []}
    end
  end
end
