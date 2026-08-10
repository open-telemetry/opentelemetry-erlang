defmodule OpenTelemetry.SemConv.Incubating.Metrics.OpenshiftMetrics do
  # This is an auto-generated file
  @moduledoc """
  OpenTelemetry Semantic Conventions for Openshift metrics.
  """
  @doc """
  The enforced hard limit of the resource across all projects.


  Instrument: `updowncounter`
  Unit: `{cpu}`
  ### Notes

  This metric is retrieved from the `Status.Total.Hard` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core)
  of the
  [ClusterResourceQuota](https://docs.redhat.com/en/documentation/openshift_container_platform/4.19/html/schedule_and_quota_apis/clusterresourcequota-quota-openshift-io-v1#status-total).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OpenshiftMetrics.openshift_clusterquota_cpu_limit_hard()
      :"openshift.clusterquota.cpu.limit.hard"

  ### Erlang

  ```erlang
  ?OPENSHIFT_CLUSTERQUOTA_CPU_LIMIT_HARD.
  'openshift.clusterquota.cpu.limit.hard'
  ```

  <!-- tabs-close -->
  """

  @spec openshift_clusterquota_cpu_limit_hard :: :"openshift.clusterquota.cpu.limit.hard"
  def openshift_clusterquota_cpu_limit_hard do
    :"openshift.clusterquota.cpu.limit.hard"
  end

  @doc """
  The current observed total usage of the resource across all projects.


  Instrument: `updowncounter`
  Unit: `{cpu}`
  ### Notes

  This metric is retrieved from the `Status.Total.Used` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core)
  of the
  [ClusterResourceQuota](https://docs.redhat.com/en/documentation/openshift_container_platform/4.19/html/schedule_and_quota_apis/clusterresourcequota-quota-openshift-io-v1#status-total).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OpenshiftMetrics.openshift_clusterquota_cpu_limit_used()
      :"openshift.clusterquota.cpu.limit.used"

  ### Erlang

  ```erlang
  ?OPENSHIFT_CLUSTERQUOTA_CPU_LIMIT_USED.
  'openshift.clusterquota.cpu.limit.used'
  ```

  <!-- tabs-close -->
  """

  @spec openshift_clusterquota_cpu_limit_used :: :"openshift.clusterquota.cpu.limit.used"
  def openshift_clusterquota_cpu_limit_used do
    :"openshift.clusterquota.cpu.limit.used"
  end

  @doc """
  The enforced hard limit of the resource across all projects.


  Instrument: `updowncounter`
  Unit: `{cpu}`
  ### Notes

  This metric is retrieved from the `Status.Total.Hard` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core)
  of the
  [ClusterResourceQuota](https://docs.redhat.com/en/documentation/openshift_container_platform/4.19/html/schedule_and_quota_apis/clusterresourcequota-quota-openshift-io-v1#status-total).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OpenshiftMetrics.openshift_clusterquota_cpu_request_hard()
      :"openshift.clusterquota.cpu.request.hard"

  ### Erlang

  ```erlang
  ?OPENSHIFT_CLUSTERQUOTA_CPU_REQUEST_HARD.
  'openshift.clusterquota.cpu.request.hard'
  ```

  <!-- tabs-close -->
  """

  @spec openshift_clusterquota_cpu_request_hard :: :"openshift.clusterquota.cpu.request.hard"
  def openshift_clusterquota_cpu_request_hard do
    :"openshift.clusterquota.cpu.request.hard"
  end

  @doc """
  The current observed total usage of the resource across all projects.


  Instrument: `updowncounter`
  Unit: `{cpu}`
  ### Notes

  This metric is retrieved from the `Status.Total.Used` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core)
  of the
  [ClusterResourceQuota](https://docs.redhat.com/en/documentation/openshift_container_platform/4.19/html/schedule_and_quota_apis/clusterresourcequota-quota-openshift-io-v1#status-total).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OpenshiftMetrics.openshift_clusterquota_cpu_request_used()
      :"openshift.clusterquota.cpu.request.used"

  ### Erlang

  ```erlang
  ?OPENSHIFT_CLUSTERQUOTA_CPU_REQUEST_USED.
  'openshift.clusterquota.cpu.request.used'
  ```

  <!-- tabs-close -->
  """

  @spec openshift_clusterquota_cpu_request_used :: :"openshift.clusterquota.cpu.request.used"
  def openshift_clusterquota_cpu_request_used do
    :"openshift.clusterquota.cpu.request.used"
  end

  @doc """
  The enforced hard limit of the resource across all projects.


  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is retrieved from the `Status.Total.Hard` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core)
  of the
  [ClusterResourceQuota](https://docs.redhat.com/en/documentation/openshift_container_platform/4.19/html/schedule_and_quota_apis/clusterresourcequota-quota-openshift-io-v1#status-total).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OpenshiftMetrics.openshift_clusterquota_ephemeral_storage_limit_hard()
      :"openshift.clusterquota.ephemeral_storage.limit.hard"

  ### Erlang

  ```erlang
  ?OPENSHIFT_CLUSTERQUOTA_EPHEMERAL_STORAGE_LIMIT_HARD.
  'openshift.clusterquota.ephemeral_storage.limit.hard'
  ```

  <!-- tabs-close -->
  """

  @spec openshift_clusterquota_ephemeral_storage_limit_hard ::
          :"openshift.clusterquota.ephemeral_storage.limit.hard"
  def openshift_clusterquota_ephemeral_storage_limit_hard do
    :"openshift.clusterquota.ephemeral_storage.limit.hard"
  end

  @doc """
  The current observed total usage of the resource across all projects.


  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is retrieved from the `Status.Total.Used` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core)
  of the
  [ClusterResourceQuota](https://docs.redhat.com/en/documentation/openshift_container_platform/4.19/html/schedule_and_quota_apis/clusterresourcequota-quota-openshift-io-v1#status-total).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OpenshiftMetrics.openshift_clusterquota_ephemeral_storage_limit_used()
      :"openshift.clusterquota.ephemeral_storage.limit.used"

  ### Erlang

  ```erlang
  ?OPENSHIFT_CLUSTERQUOTA_EPHEMERAL_STORAGE_LIMIT_USED.
  'openshift.clusterquota.ephemeral_storage.limit.used'
  ```

  <!-- tabs-close -->
  """

  @spec openshift_clusterquota_ephemeral_storage_limit_used ::
          :"openshift.clusterquota.ephemeral_storage.limit.used"
  def openshift_clusterquota_ephemeral_storage_limit_used do
    :"openshift.clusterquota.ephemeral_storage.limit.used"
  end

  @doc """
  The enforced hard limit of the resource across all projects.


  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is retrieved from the `Status.Total.Hard` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core)
  of the
  [ClusterResourceQuota](https://docs.redhat.com/en/documentation/openshift_container_platform/4.19/html/schedule_and_quota_apis/clusterresourcequota-quota-openshift-io-v1#status-total).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OpenshiftMetrics.openshift_clusterquota_ephemeral_storage_request_hard()
      :"openshift.clusterquota.ephemeral_storage.request.hard"

  ### Erlang

  ```erlang
  ?OPENSHIFT_CLUSTERQUOTA_EPHEMERAL_STORAGE_REQUEST_HARD.
  'openshift.clusterquota.ephemeral_storage.request.hard'
  ```

  <!-- tabs-close -->
  """

  @spec openshift_clusterquota_ephemeral_storage_request_hard ::
          :"openshift.clusterquota.ephemeral_storage.request.hard"
  def openshift_clusterquota_ephemeral_storage_request_hard do
    :"openshift.clusterquota.ephemeral_storage.request.hard"
  end

  @doc """
  The current observed total usage of the resource across all projects.


  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is retrieved from the `Status.Total.Used` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core)
  of the
  [ClusterResourceQuota](https://docs.redhat.com/en/documentation/openshift_container_platform/4.19/html/schedule_and_quota_apis/clusterresourcequota-quota-openshift-io-v1#status-total).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OpenshiftMetrics.openshift_clusterquota_ephemeral_storage_request_used()
      :"openshift.clusterquota.ephemeral_storage.request.used"

  ### Erlang

  ```erlang
  ?OPENSHIFT_CLUSTERQUOTA_EPHEMERAL_STORAGE_REQUEST_USED.
  'openshift.clusterquota.ephemeral_storage.request.used'
  ```

  <!-- tabs-close -->
  """

  @spec openshift_clusterquota_ephemeral_storage_request_used ::
          :"openshift.clusterquota.ephemeral_storage.request.used"
  def openshift_clusterquota_ephemeral_storage_request_used do
    :"openshift.clusterquota.ephemeral_storage.request.used"
  end

  @doc """
  The enforced hard limit of the resource across all projects.


  Instrument: `updowncounter`
  Unit: `{hugepage}`
  ### Notes

  This metric is retrieved from the `Status.Total.Hard` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core)
  of the
  [ClusterResourceQuota](https://docs.redhat.com/en/documentation/openshift_container_platform/4.19/html/schedule_and_quota_apis/clusterresourcequota-quota-openshift-io-v1#status-total).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OpenshiftMetrics.openshift_clusterquota_hugepage_count_request_hard()
      :"openshift.clusterquota.hugepage_count.request.hard"

  ### Erlang

  ```erlang
  ?OPENSHIFT_CLUSTERQUOTA_HUGEPAGE_COUNT_REQUEST_HARD.
  'openshift.clusterquota.hugepage_count.request.hard'
  ```

  <!-- tabs-close -->
  """

  @spec openshift_clusterquota_hugepage_count_request_hard ::
          :"openshift.clusterquota.hugepage_count.request.hard"
  def openshift_clusterquota_hugepage_count_request_hard do
    :"openshift.clusterquota.hugepage_count.request.hard"
  end

  @doc """
  The current observed total usage of the resource across all projects.


  Instrument: `updowncounter`
  Unit: `{hugepage}`
  ### Notes

  This metric is retrieved from the `Status.Total.Used` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core)
  of the
  [ClusterResourceQuota](https://docs.redhat.com/en/documentation/openshift_container_platform/4.19/html/schedule_and_quota_apis/clusterresourcequota-quota-openshift-io-v1#status-total).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OpenshiftMetrics.openshift_clusterquota_hugepage_count_request_used()
      :"openshift.clusterquota.hugepage_count.request.used"

  ### Erlang

  ```erlang
  ?OPENSHIFT_CLUSTERQUOTA_HUGEPAGE_COUNT_REQUEST_USED.
  'openshift.clusterquota.hugepage_count.request.used'
  ```

  <!-- tabs-close -->
  """

  @spec openshift_clusterquota_hugepage_count_request_used ::
          :"openshift.clusterquota.hugepage_count.request.used"
  def openshift_clusterquota_hugepage_count_request_used do
    :"openshift.clusterquota.hugepage_count.request.used"
  end

  @doc """
  The enforced hard limit of the resource across all projects.


  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is retrieved from the `Status.Total.Hard` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core)
  of the
  [ClusterResourceQuota](https://docs.redhat.com/en/documentation/openshift_container_platform/4.19/html/schedule_and_quota_apis/clusterresourcequota-quota-openshift-io-v1#status-total).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OpenshiftMetrics.openshift_clusterquota_memory_limit_hard()
      :"openshift.clusterquota.memory.limit.hard"

  ### Erlang

  ```erlang
  ?OPENSHIFT_CLUSTERQUOTA_MEMORY_LIMIT_HARD.
  'openshift.clusterquota.memory.limit.hard'
  ```

  <!-- tabs-close -->
  """

  @spec openshift_clusterquota_memory_limit_hard :: :"openshift.clusterquota.memory.limit.hard"
  def openshift_clusterquota_memory_limit_hard do
    :"openshift.clusterquota.memory.limit.hard"
  end

  @doc """
  The current observed total usage of the resource across all projects.


  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is retrieved from the `Status.Total.Used` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core)
  of the
  [ClusterResourceQuota](https://docs.redhat.com/en/documentation/openshift_container_platform/4.19/html/schedule_and_quota_apis/clusterresourcequota-quota-openshift-io-v1#status-total).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OpenshiftMetrics.openshift_clusterquota_memory_limit_used()
      :"openshift.clusterquota.memory.limit.used"

  ### Erlang

  ```erlang
  ?OPENSHIFT_CLUSTERQUOTA_MEMORY_LIMIT_USED.
  'openshift.clusterquota.memory.limit.used'
  ```

  <!-- tabs-close -->
  """

  @spec openshift_clusterquota_memory_limit_used :: :"openshift.clusterquota.memory.limit.used"
  def openshift_clusterquota_memory_limit_used do
    :"openshift.clusterquota.memory.limit.used"
  end

  @doc """
  The enforced hard limit of the resource across all projects.


  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is retrieved from the `Status.Total.Hard` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core)
  of the
  [ClusterResourceQuota](https://docs.redhat.com/en/documentation/openshift_container_platform/4.19/html/schedule_and_quota_apis/clusterresourcequota-quota-openshift-io-v1#status-total).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OpenshiftMetrics.openshift_clusterquota_memory_request_hard()
      :"openshift.clusterquota.memory.request.hard"

  ### Erlang

  ```erlang
  ?OPENSHIFT_CLUSTERQUOTA_MEMORY_REQUEST_HARD.
  'openshift.clusterquota.memory.request.hard'
  ```

  <!-- tabs-close -->
  """

  @spec openshift_clusterquota_memory_request_hard ::
          :"openshift.clusterquota.memory.request.hard"
  def openshift_clusterquota_memory_request_hard do
    :"openshift.clusterquota.memory.request.hard"
  end

  @doc """
  The current observed total usage of the resource across all projects.


  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is retrieved from the `Status.Total.Used` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core)
  of the
  [ClusterResourceQuota](https://docs.redhat.com/en/documentation/openshift_container_platform/4.19/html/schedule_and_quota_apis/clusterresourcequota-quota-openshift-io-v1#status-total).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OpenshiftMetrics.openshift_clusterquota_memory_request_used()
      :"openshift.clusterquota.memory.request.used"

  ### Erlang

  ```erlang
  ?OPENSHIFT_CLUSTERQUOTA_MEMORY_REQUEST_USED.
  'openshift.clusterquota.memory.request.used'
  ```

  <!-- tabs-close -->
  """

  @spec openshift_clusterquota_memory_request_used ::
          :"openshift.clusterquota.memory.request.used"
  def openshift_clusterquota_memory_request_used do
    :"openshift.clusterquota.memory.request.used"
  end

  @doc """
  The enforced hard limit of the resource across all projects.


  Instrument: `updowncounter`
  Unit: `{object}`
  ### Notes

  This metric is retrieved from the `Status.Total.Hard` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core)
  of the
  [ClusterResourceQuota](https://docs.redhat.com/en/documentation/openshift_container_platform/4.19/html/schedule_and_quota_apis/clusterresourcequota-quota-openshift-io-v1#status-total).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OpenshiftMetrics.openshift_clusterquota_object_count_hard()
      :"openshift.clusterquota.object_count.hard"

  ### Erlang

  ```erlang
  ?OPENSHIFT_CLUSTERQUOTA_OBJECT_COUNT_HARD.
  'openshift.clusterquota.object_count.hard'
  ```

  <!-- tabs-close -->
  """

  @spec openshift_clusterquota_object_count_hard :: :"openshift.clusterquota.object_count.hard"
  def openshift_clusterquota_object_count_hard do
    :"openshift.clusterquota.object_count.hard"
  end

  @doc """
  The current observed total usage of the resource across all projects.


  Instrument: `updowncounter`
  Unit: `{object}`
  ### Notes

  This metric is retrieved from the `Status.Total.Used` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core)
  of the
  [ClusterResourceQuota](https://docs.redhat.com/en/documentation/openshift_container_platform/4.19/html/schedule_and_quota_apis/clusterresourcequota-quota-openshift-io-v1#status-total).


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OpenshiftMetrics.openshift_clusterquota_object_count_used()
      :"openshift.clusterquota.object_count.used"

  ### Erlang

  ```erlang
  ?OPENSHIFT_CLUSTERQUOTA_OBJECT_COUNT_USED.
  'openshift.clusterquota.object_count.used'
  ```

  <!-- tabs-close -->
  """

  @spec openshift_clusterquota_object_count_used :: :"openshift.clusterquota.object_count.used"
  def openshift_clusterquota_object_count_used do
    :"openshift.clusterquota.object_count.used"
  end

  @doc """
  The enforced hard limit of the resource across all projects.


  Instrument: `updowncounter`
  Unit: `{persistentvolumeclaim}`
  ### Notes

  This metric is retrieved from the `Status.Total.Hard` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core)
  of the
  [ClusterResourceQuota](https://docs.redhat.com/en/documentation/openshift_container_platform/4.19/html/schedule_and_quota_apis/clusterresourcequota-quota-openshift-io-v1#status-total).

  The `k8s.storageclass.name` should be required when a resource quota is defined for a specific
  storage class.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OpenshiftMetrics.openshift_clusterquota_persistentvolumeclaim_count_hard()
      :"openshift.clusterquota.persistentvolumeclaim_count.hard"

  ### Erlang

  ```erlang
  ?OPENSHIFT_CLUSTERQUOTA_PERSISTENTVOLUMECLAIM_COUNT_HARD.
  'openshift.clusterquota.persistentvolumeclaim_count.hard'
  ```

  <!-- tabs-close -->
  """

  @spec openshift_clusterquota_persistentvolumeclaim_count_hard ::
          :"openshift.clusterquota.persistentvolumeclaim_count.hard"
  def openshift_clusterquota_persistentvolumeclaim_count_hard do
    :"openshift.clusterquota.persistentvolumeclaim_count.hard"
  end

  @doc """
  The current observed total usage of the resource across all projects.


  Instrument: `updowncounter`
  Unit: `{persistentvolumeclaim}`
  ### Notes

  This metric is retrieved from the `Status.Total.Used` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core)
  of the
  [ClusterResourceQuota](https://docs.redhat.com/en/documentation/openshift_container_platform/4.19/html/schedule_and_quota_apis/clusterresourcequota-quota-openshift-io-v1#status-total).

  The `k8s.storageclass.name` should be required when a resource quota is defined for a specific
  storage class.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OpenshiftMetrics.openshift_clusterquota_persistentvolumeclaim_count_used()
      :"openshift.clusterquota.persistentvolumeclaim_count.used"

  ### Erlang

  ```erlang
  ?OPENSHIFT_CLUSTERQUOTA_PERSISTENTVOLUMECLAIM_COUNT_USED.
  'openshift.clusterquota.persistentvolumeclaim_count.used'
  ```

  <!-- tabs-close -->
  """

  @spec openshift_clusterquota_persistentvolumeclaim_count_used ::
          :"openshift.clusterquota.persistentvolumeclaim_count.used"
  def openshift_clusterquota_persistentvolumeclaim_count_used do
    :"openshift.clusterquota.persistentvolumeclaim_count.used"
  end

  @doc """
  The enforced hard limit of the resource across all projects.


  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is retrieved from the `Status.Total.Hard` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core)
  of the
  [ClusterResourceQuota](https://docs.redhat.com/en/documentation/openshift_container_platform/4.19/html/schedule_and_quota_apis/clusterresourcequota-quota-openshift-io-v1#status-total).

  The `k8s.storageclass.name` should be required when a resource quota is defined for a specific
  storage class.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OpenshiftMetrics.openshift_clusterquota_storage_request_hard()
      :"openshift.clusterquota.storage.request.hard"

  ### Erlang

  ```erlang
  ?OPENSHIFT_CLUSTERQUOTA_STORAGE_REQUEST_HARD.
  'openshift.clusterquota.storage.request.hard'
  ```

  <!-- tabs-close -->
  """

  @spec openshift_clusterquota_storage_request_hard ::
          :"openshift.clusterquota.storage.request.hard"
  def openshift_clusterquota_storage_request_hard do
    :"openshift.clusterquota.storage.request.hard"
  end

  @doc """
  The current observed total usage of the resource across all projects.


  Instrument: `updowncounter`
  Unit: `By`
  ### Notes

  This metric is retrieved from the `Status.Total.Used` field of the
  [K8s ResourceQuotaStatus](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.32/#resourcequotastatus-v1-core)
  of the
  [ClusterResourceQuota](https://docs.redhat.com/en/documentation/openshift_container_platform/4.19/html/schedule_and_quota_apis/clusterresourcequota-quota-openshift-io-v1#status-total).

  The `k8s.storageclass.name` should be required when a resource quota is defined for a specific
  storage class.


  <!-- tabs-open -->
  ### Elixir

      iex> OpenTelemetry.SemConv.Incubating.Metrics.OpenshiftMetrics.openshift_clusterquota_storage_request_used()
      :"openshift.clusterquota.storage.request.used"

  ### Erlang

  ```erlang
  ?OPENSHIFT_CLUSTERQUOTA_STORAGE_REQUEST_USED.
  'openshift.clusterquota.storage.request.used'
  ```

  <!-- tabs-close -->
  """

  @spec openshift_clusterquota_storage_request_used ::
          :"openshift.clusterquota.storage.request.used"
  def openshift_clusterquota_storage_request_used do
    :"openshift.clusterquota.storage.request.used"
  end
end
