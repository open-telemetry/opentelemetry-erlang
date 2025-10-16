changelogs:
    #!/usr/bin/env bash
    set -euxo pipefail

    for dir in apps/*; do
      if [ -d "$dir" ]; then # Check if it's actually a directory
        echo "Processing directory: ${dir}/**"

        # version tag pattern is specific to each sub-app in apps/
        # create the pattern based on the name of the directory with
        # "opentelemetry" dropped from it and prepended with the generic
        # pattern matching v followed by an integer
        IFS='_' read -r -a parts <<< "$dir"
        parts=("${parts[@]:1}")
        parts+=("v[0-9].*")
        tag=$(IFS='-'; echo "${parts[*]}")

        git cliff 29b747722addf39ce40014ee1a905a973c9044be.. --include-path "${dir}/**" --workdir ${dir} -o ${dir}/CHANGELOG.md --tag-pattern "${tag}" --config ../../cliff.toml
      fi
    done

# lint the commits between HEAD and the point from `origin/main` this branch was created from
commit-lint:
    npm install -D @commitlint/cli @commitlint/config-conventional
    npx commitlint --verbose --from `git merge-base --fork-point origin/main` --to HEAD
