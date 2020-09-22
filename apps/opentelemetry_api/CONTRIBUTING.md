# Contributing to opentelemetry-erlang-api

The Erlang/Elixir special interest group (SIG) meets as part of the 
[Erlang Ecosystem Foundation](https://erlef.org/wg/observability). See the
OpenTelemetry [community](https://github.com/open-telemetry/community#erlangelixir-sdk)
repo for information on this and other language SIGs.


## Pull Requests

### How to Send Pull Requests

Everyone is welcome to contribute code to `opentelemetry-erlang-api` via
GitHub pull requests (PRs).

To create a new PR, fork the project in GitHub and clone the upstream
repo:

```sh
$ git clone https://github.com/open-telemetry/opentelemetry-erlang-api/
```

This would put the project in the `opentelemetry-erlang-api` directory in
current working directory.

Enter the newly created directory and add your fork as a new remote:

```sh
$ git remote add <YOUR_FORK> git@github.com:<YOUR_GITHUB_USERNAME>/opentelemetry-erlang-api
```

Check out a new branch, make modifications, run tests, and
push the branch to your fork:

```sh
$ git checkout -b <YOUR_BRANCH_NAME>
# edit files
$ rebar3 ct
$ mix test
$ git add -p
$ git commit
$ git push <YOUR_FORK> <YOUR_BRANCH_NAME>
```

Open a pull request against the main `opentelemetry-erlang-api` repo.

### How to Receive Comments

* If the PR is not ready for review, please put `[WIP]` in the title,
  tag it as `work-in-progress`, or mark it as
  [`draft`](https://github.blog/2019-02-14-introducing-draft-pull-requests/).
* Make sure CLA is signed and CI is clear.

### How to Get PRs Merged

A PR is considered to be **ready to merge** when:

* It has received two approvals from Collaborators/Maintainers (at
  different companies).
* Major feedbacks are resolved.
* It has been open for review for at least one working day. This gives
  people reasonable time to review.
* Trivial change (typo, cosmetic, doc, etc.) doesn't have to wait for
  one day.
* Urgent fix can take exception as long as it has been actively
  communicated.

Any Collaborator/Maintainer can merge the PR once it is **ready to
merge**.

## Design Choices

As with other OpenTelemetry clients, opentelemetry-erlang-api follows the
[opentelemetry-specification](https://github.com/open-telemetry/opentelemetry-specification).

It's especially valuable to read through the [library
guidelines](https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/library-guidelines.md).

### Focus on Capabilities, Not Structure Compliance

OpenTelemetry is an evolving specification, one where the desires and
use cases are clear, but the method to satisfy those uses cases are
not.

As such, Contributions should provide functionality and behavior that
conforms to the specification, but the interface and structure is
flexible.

It is preferable to have contributions follow the idioms of the
language rather than conform to specific API names or argument
patterns in the spec.

For a deeper discussion, see:
https://github.com/open-telemetry/opentelemetry-specification/issues/165

## Style Guide

* Make sure to run `make precommit` - this will find and fix the code
  formatting.

## Approvers and Maintainers

Approvers:

- [Fred Hebert](https://github.com/ferd), Postmates
- [Greg Mefford](https://github.com/gregmefford), Bleacher Report
- [Zach Daniel](https://github.com/zachdaniel), Variance

Maintainers:

- [Tristan Sloughter](https://github.com/tsloughter), Postmates
- [Ilya Khaprov](https://github.com/deadtrickster), Kobil Systems GmbH 
- [Łukasz Jan Niemier](https://github.com/hauleth), Kobil Systems GmbH 

### Become an Approver or a Maintainer

See the [community membership document in OpenTelemetry community
repo](https://github.com/open-telemetry/community/blob/master/community-membership.md).
