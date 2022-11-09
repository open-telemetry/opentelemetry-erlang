#!/usr/bin/env bash

set -ex

semconv_version='1.13.0'
docker_img_vsn='0.14.0'
schema_uri=https://opentelemetry.io/schemas/$semconv_version

tmp_dir=$(mktemp -d)
git clone https://github.com/open-telemetry/opentelemetry-specification.git $tmp_dir

cwd=$(pwd)

cd $tmp_dir

git checkout "v${semconv_version}"

for kind in trace resource
do
    docker run --rm \
           -v "${tmp_dir}/semantic_conventions/${kind}":/source \
           -v "${cwd}/templates":/templates \
           -v "${cwd}/include":/output \
           otel/semconvgen:${docker_img_vsn} \
           -f /source code \
           --template /templates/semantic_conventions.hrl.j2 \
           --output /output/${kind}.hrl \
           -Dmodule=${kind} \
           -Dschema_uri=${schema_uri}
done
