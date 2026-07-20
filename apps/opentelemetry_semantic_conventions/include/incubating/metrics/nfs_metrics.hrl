
%%%------------------------------------------------------------------------
%% Copyright The OpenTelemetry Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%%-------------------------------------------------------------------------

%% Reports the count of kernel NFS client TCP segments and UDP datagrams handled.
-define(NFS_CLIENT_NET_COUNT, 'nfs.client.net.count').


%% Reports the count of kernel NFS client TCP connections accepted.
-define(NFS_CLIENT_NET_TCP_CONNECTION_ACCEPTED, 'nfs.client.net.tcp.connection.accepted').


%% Reports the count of kernel NFSv4+ client operations.
-define(NFS_CLIENT_OPERATION_COUNT, 'nfs.client.operation.count').


%% Reports the count of kernel NFS client procedures.
-define(NFS_CLIENT_PROCEDURE_COUNT, 'nfs.client.procedure.count').


%% Reports the count of kernel NFS client RPC authentication refreshes.
-define(NFS_CLIENT_RPC_AUTHREFRESH_COUNT, 'nfs.client.rpc.authrefresh.count').


%% Reports the count of kernel NFS client RPCs sent, regardless of whether they're accepted/rejected by the server.
-define(NFS_CLIENT_RPC_COUNT, 'nfs.client.rpc.count').


%% Reports the count of kernel NFS client RPC retransmits.
-define(NFS_CLIENT_RPC_RETRANSMIT_COUNT, 'nfs.client.rpc.retransmit.count').


%% Reports the count of kernel NFS server stale file handles.
-define(NFS_SERVER_FH_STALE_COUNT, 'nfs.server.fh.stale.count').


%% Reports the count of kernel NFS server bytes returned to receive and transmit (read and write) requests.
-define(NFS_SERVER_IO, 'nfs.server.io').


%% Reports the count of kernel NFS server TCP segments and UDP datagrams handled.
-define(NFS_SERVER_NET_COUNT, 'nfs.server.net.count').


%% Reports the count of kernel NFS server TCP connections accepted.
-define(NFS_SERVER_NET_TCP_CONNECTION_ACCEPTED, 'nfs.server.net.tcp.connection.accepted').


%% Reports the count of kernel NFSv4+ server operations.
-define(NFS_SERVER_OPERATION_COUNT, 'nfs.server.operation.count').


%% Reports the count of kernel NFS server procedures.
-define(NFS_SERVER_PROCEDURE_COUNT, 'nfs.server.procedure.count').


%% Reports the kernel NFS server reply cache request count by cache hit status.
-define(NFS_SERVER_REPCACHE_REQUESTS, 'nfs.server.repcache.requests').


%% Reports the count of kernel NFS server RPCs handled.
-define(NFS_SERVER_RPC_COUNT, 'nfs.server.rpc.count').


%% Reports the count of kernel NFS server available threads.
-define(NFS_SERVER_THREAD_COUNT, 'nfs.server.thread.count').
