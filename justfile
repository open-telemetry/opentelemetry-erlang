changelogs:
    #!/usr/bin/env bash
    set -euxo pipefail

    for dir in apps/*; do
      if [ -d "$dir" ]; then # Check if it's actually a directory
        echo "Processing directory: ${dir}/**"

        # version tag is <app>/v<vsn>
        tag="${dir#apps/}/v[0-9].*"

        git cliff 29b747722addf39ce40014ee1a905a973c9044be.. \
            --skip-commit 9f0511e705f18e4b3cc1767b42367ba49e02455a \
            --skip-commit 1f471ab19cac62ef2f88b9fc78a345113c984ef9 \
            --include-path "${dir}/**" \
            --workdir ${dir} \
            -o ${dir}/CHANGELOG.md \
            --tag-pattern "${tag}" \
            --config ../../cliff.toml
      fi
    done

# lint the commits between HEAD and the point from `origin/main` this branch was created from
commit-lint:
    npm install -D @commitlint/cli @commitlint/config-conventional
    npx commitlint --verbose --from `git merge-base --fork-point origin/main` --to HEAD
