defmodule OpenTelemetry.SemConv.Incubating.ThreadAttributes do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Thread attributes.
  """

  @doc """
  Current "managed" thread ID (as opposed to OS thread ID).

  ### Value type

  Value must be of type `integer()`.
  ### Notes


  Examples of where the value can be extracted from:

  | Language or platform | Source |
  | --- | --- |
  | JVM | `Thread.currentThread().threadId()` |
  | .NET | `Thread.CurrentThread.ManagedThreadId` |
  | Python | `threading.current_thread().ident` |
  | Ruby | `Thread.current.object_id` |
  | C++ | `std::this_thread::get_id()` |
  | Erlang | `erlang:self()` |

  ### Examples

  ```
  42
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ThreadAttributes.thread_id()
      :"thread.id"

  ### Erlang

  ```erlang
  ?THREAD_ID.
  'thread.id'
  ```

  <!-- tabs-close -->
  """
  @spec thread_id :: :"thread.id"
  def thread_id do
    :"thread.id"
  end

  @doc """
  Current thread name.

  ### Value type

  Value must be of type `atom() | String.t()`.
  ### Notes


  Examples of where the value can be extracted from:

  | Language or platform | Source |
  | --- | --- |
  | JVM | `Thread.currentThread().getName()` |
  | .NET | `Thread.CurrentThread.Name` |
  | Python | `threading.current_thread().name` |
  | Ruby | `Thread.current.name` |
  | Erlang | `erlang:process_info(self(), registered_name)` |

  ### Examples

  ```
  main
  ```

  <!-- tabs-open -->

  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.ThreadAttributes.thread_name()
      :"thread.name"

  ### Erlang

  ```erlang
  ?THREAD_NAME.
  'thread.name'
  ```

  <!-- tabs-close -->
  """
  @spec thread_name :: :"thread.name"
  def thread_name do
    :"thread.name"
  end
end
