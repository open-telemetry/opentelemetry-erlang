#!/usr/bin/env bash

set -ex

semconv_version='1.25.0'
docker_img_vsn='0.24.0'
schema_uri=https://opentelemetry.io/schemas/$semconv_version

tmp_dir=$(mktemp -d)
git clone https://github.com/open-telemetry/opentelemetry-specification.git $tmp_dir

cwd=$(pwd)

cd $tmp_dir

git checkout "v${semconv_version}"

ls -al

for kind in trace resource logs metrics; do
  echo "generating ${kind}"
  echo $tmp_dir
  docker run --rm \
    -v "${tmp_dir}/semantic-conventions/model/${kind}":/source \
    -v "${cwd}/templates":/templates \
    -v "${cwd}/include":/output \
    otel/semconvgen:${docker_img_vsn} \
    -f /source code \
    --template /templates/semantic_conventions.hrl.j2 \
    --output /output/${kind}.hrl \
    -Dmodule=${kind} \
    -Dschema_uri=${schema_uri}

  docker run --rm \
    -v "${tmp_dir}/semantic-conventions/model/${kind}":/source \
    -v "${cwd}/templates":/templates \
    -v "${cwd}/lib/open_telemetry/semantic_conventions":/output \
    otel/semconvgen:${docker_img_vsn} \
    -f /source code \
    --template /templates/semantic_conventions.ex.j2 \
    --output /output/${kind}.ex \
    -Dmodule=${kind} \
    -Dschema_uri=${schema_uri}
done
