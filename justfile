bump dir:
    #!/usr/bin/env bash
    set -euxo pipefail

    dir={{dir}}

    prefix="${dir#apps/}/v"
    tag="${dir#apps/}[0-9].*"

    vsn=$(just _cliff {{dir}} "${tag}" --bumped-version --unreleased) || echo "No tag found. Input a new tag:" && read vsn

    #echo "${vsn#$prefix}" > "${dir}/VERSION"
    echo "${vsn#$prefix}"


changelogs:
    #!/usr/bin/env bash
    set -euxo pipefail

    for dir in apps/*; do
      if [ -d "$dir" ]; then # Check if it's actually a directory
        echo "Processing directory: ${dir}/**"
        just changelog $dir
      fi
    done

changelog dir:
    #!/usr/bin/env bash
    set -euxo pipefail

    dir="{{dir}}"
    tag="${dir#apps/}/v[0-9].*"

    just _cliff "{{dir}}" "${tag}" -o {{dir}}/CHANGELOG.md

# lint the commits between HEAD and the point from `origin/main` this branch was created from
commit-lint:
    npm install -D @commitlint/cli @commitlint/config-conventional
    npx commitlint --verbose --from `git merge-base --fork-point origin/main` --to HEAD

_cliff dir pattern *args:
    @git cliff 29b747722addf39ce40014ee1a905a973c9044be.. --include-path "{{dir}}/**" --workdir {{dir}} --tag-pattern "{{pattern}}" --config ../../cliff.toml {{args}}
