
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

%% The provenance filename of the built attestation which directly relates to the build artifact filename. This filename SHOULD accompany the artifact at publish time. See the [SLSA Relationship](https://slsa.dev/spec/v1.0/distributing-provenance#relationship-between-artifacts-and-attestations) specification for more information.
%%  
-define(ARTIFACT_ATTESTATION_FILENAME, 'artifact.attestation.filename').


%% The full [hash value (see glossary)](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.186-5.pdf), of the built attestation. Some envelopes in the software attestation space also refer to this as the [digest](https://github.com/in-toto/attestation/blob/main/spec/README.md#in-toto-attestation-framework-spec).
%%  
-define(ARTIFACT_ATTESTATION_HASH, 'artifact.attestation.hash').


%% The id of the build [software attestation](https://slsa.dev/attestation-model).
%%  
-define(ARTIFACT_ATTESTATION_ID, 'artifact.attestation.id').


%% The human readable file name of the artifact, typically generated during build and release processes. Often includes the package name and version in the file name.
%%  
-define(ARTIFACT_FILENAME, 'artifact.filename').


%% The full [hash value (see glossary)](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.186-5.pdf), often found in checksum.txt on a release of the artifact and used to verify package integrity.
%%  
-define(ARTIFACT_HASH, 'artifact.hash').


%% The [Package URL](https://github.com/package-url/purl-spec) of the [package artifact](https://slsa.dev/spec/v1.0/terminology#package-model) provides a standard way to identify and locate the packaged artifact.
%%  
-define(ARTIFACT_PURL, 'artifact.purl').


%% The version of the artifact.
%%  
-define(ARTIFACT_VERSION, 'artifact.version').
