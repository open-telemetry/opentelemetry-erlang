defmodule OpenTelemetry.SemConv.Incubating.Metrics.VCSMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for VCS metrics.
  """
  @doc """
  The number of changes (pull requests/merge requests/changelists) in a repository, categorized by their state (e.g. open or merged)

  Instrument: `updowncounter`
  Unit: `{change}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.VCSMetrics.vcs_change_count()
      :"vcs.change.count"

  ### Erlang

  ```erlang
  ?VCS_CHANGE_COUNT.
  'vcs.change.count'
  ```

  <!-- tabs-close -->
  """

  @spec vcs_change_count :: :"vcs.change.count"
  def vcs_change_count do
    :"vcs.change.count"
  end

  @doc """
  The time duration a change (pull request/merge request/changelist) has been in a given state.

  Instrument: `gauge`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.VCSMetrics.vcs_change_duration()
      :"vcs.change.duration"

  ### Erlang

  ```erlang
  ?VCS_CHANGE_DURATION.
  'vcs.change.duration'
  ```

  <!-- tabs-close -->
  """

  @spec vcs_change_duration :: :"vcs.change.duration"
  def vcs_change_duration do
    :"vcs.change.duration"
  end

  @doc """
  The amount of time since its creation it took a change (pull request/merge request/changelist) to get the first approval.

  Instrument: `gauge`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.VCSMetrics.vcs_change_time_to_approval()
      :"vcs.change.time_to_approval"

  ### Erlang

  ```erlang
  ?VCS_CHANGE_TIME_TO_APPROVAL.
  'vcs.change.time_to_approval'
  ```

  <!-- tabs-close -->
  """

  @spec vcs_change_time_to_approval :: :"vcs.change.time_to_approval"
  def vcs_change_time_to_approval do
    :"vcs.change.time_to_approval"
  end

  @doc """
  The amount of time since its creation it took a change (pull request/merge request/changelist) to get merged into the target(base) ref.

  Instrument: `gauge`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.VCSMetrics.vcs_change_time_to_merge()
      :"vcs.change.time_to_merge"

  ### Erlang

  ```erlang
  ?VCS_CHANGE_TIME_TO_MERGE.
  'vcs.change.time_to_merge'
  ```

  <!-- tabs-close -->
  """

  @spec vcs_change_time_to_merge :: :"vcs.change.time_to_merge"
  def vcs_change_time_to_merge do
    :"vcs.change.time_to_merge"
  end

  @doc """
  The number of unique contributors to a repository

  Instrument: `gauge`
  Unit: `{contributor}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.VCSMetrics.vcs_contributor_count()
      :"vcs.contributor.count"

  ### Erlang

  ```erlang
  ?VCS_CONTRIBUTOR_COUNT.
  'vcs.contributor.count'
  ```

  <!-- tabs-close -->
  """

  @spec vcs_contributor_count :: :"vcs.contributor.count"
  def vcs_contributor_count do
    :"vcs.contributor.count"
  end

  @doc """
  The number of refs of type branch or tag in a repository.

  Instrument: `updowncounter`
  Unit: `{ref}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.VCSMetrics.vcs_ref_count()
      :"vcs.ref.count"

  ### Erlang

  ```erlang
  ?VCS_REF_COUNT.
  'vcs.ref.count'
  ```

  <!-- tabs-close -->
  """

  @spec vcs_ref_count :: :"vcs.ref.count"
  def vcs_ref_count do
    :"vcs.ref.count"
  end

  @doc """
  The number of lines added/removed in a ref (branch) relative to the ref from the `vcs.ref.base.name` attribute.

  Instrument: `gauge`
  Unit: `{line}`
  ### Notes

  This metric should be reported for each `vcs.line_change.type` value. For example if a ref added 3 lines and removed 2 lines,
  instrumentation **SHOULD** report two measurements: 3 and 2 (both positive numbers).
  If number of lines added/removed should be calculated from the start of time, then `vcs.ref.base.name` **SHOULD** be set to an empty string.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.VCSMetrics.vcs_ref_lines_delta()
      :"vcs.ref.lines_delta"

  ### Erlang

  ```erlang
  ?VCS_REF_LINES_DELTA.
  'vcs.ref.lines_delta'
  ```

  <!-- tabs-close -->
  """

  @spec vcs_ref_lines_delta :: :"vcs.ref.lines_delta"
  def vcs_ref_lines_delta do
    :"vcs.ref.lines_delta"
  end

  @doc """
  The number of revisions (commits) a ref (branch) is ahead/behind the branch from the `vcs.ref.base.name` attribute

  Instrument: `gauge`
  Unit: `{revision}`
  ### Notes

  This metric should be reported for each `vcs.revision_delta.direction` value. For example if branch `a` is 3 commits behind and 2 commits ahead of `trunk`,
  instrumentation **SHOULD** report two measurements: 3 and 2 (both positive numbers) and `vcs.ref.base.name` is set to `trunk`.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.VCSMetrics.vcs_ref_revisions_delta()
      :"vcs.ref.revisions_delta"

  ### Erlang

  ```erlang
  ?VCS_REF_REVISIONS_DELTA.
  'vcs.ref.revisions_delta'
  ```

  <!-- tabs-close -->
  """

  @spec vcs_ref_revisions_delta :: :"vcs.ref.revisions_delta"
  def vcs_ref_revisions_delta do
    :"vcs.ref.revisions_delta"
  end

  @doc """
  Time a ref (branch) created from the default branch (trunk) has existed. The `ref.type` attribute will always be `branch`

  Instrument: `gauge`
  Unit: `s`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.VCSMetrics.vcs_ref_time()
      :"vcs.ref.time"

  ### Erlang

  ```erlang
  ?VCS_REF_TIME.
  'vcs.ref.time'
  ```

  <!-- tabs-close -->
  """

  @spec vcs_ref_time :: :"vcs.ref.time"
  def vcs_ref_time do
    :"vcs.ref.time"
  end

  @doc """
  The number of repositories in an organization.

  Instrument: `updowncounter`
  Unit: `{repository}`

  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.VCSMetrics.vcs_repository_count()
      :"vcs.repository.count"

  ### Erlang

  ```erlang
  ?VCS_REPOSITORY_COUNT.
  'vcs.repository.count'
  ```

  <!-- tabs-close -->
  """

  @spec vcs_repository_count :: :"vcs.repository.count"
  def vcs_repository_count do
    :"vcs.repository.count"
  end
end
