changelogs:
    #!/usr/bin/env bash
    set -euxo pipefail

    for dir in apps/*; do
      if [ -d "$dir" ]; then # Check if it's actually a directory
        echo "Processing directory: ${dir}/**"
        git cliff 29b747722addf39ce40014ee1a905a973c9044be.. --include-path "${dir}/**" --workdir ${dir} -o ${dir}/CHANGELOG.md
      fi
    done
