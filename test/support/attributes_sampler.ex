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
    case has_match(attributes, config_attributes) do
      true ->
        {:drop, [], []}

      _ ->
        {:record_and_sample, [], []}
    end
  end

  def has_match(a, b) do
    {min, max} =
      if map_size(a) < map_size(b) do
        {a, b}
      else
        {b, a}
      end

    i = :maps.iterator(min)
    has_match_(:maps.next(i), max)
  end

  def has_match_(:none, _), do: false
  def has_match_({k, v, _i}, b) when :erlang.map_get(k, b) === v, do: true
  def has_match_({_k, _v, i}, b), do: has_match_(:maps.next(i), b)
end
