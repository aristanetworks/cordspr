
![spr](./docs/spr.svg)

# spr &middot; [![GitHub](https://img.shields.io/github/license/getcord/spr)](https://img.shields.io/github/license/getcord/spr) [![GitHub release](https://img.shields.io/github/v/release/getcord/spr?include_prereleases)](https://github.com/getcord/spr/releases) [![crates.io](https://img.shields.io/crates/v/spr.svg)](https://crates.io/crates/spr) [![homebrew](https://img.shields.io/homebrew/v/spr.svg)](https://formulae.brew.sh/formula/spr) [![GitHub Repo stars](https://img.shields.io/github/stars/getcord/spr?style=social)](https://github.com/getcord/spr)

A command-line tool for submitting and updating GitHub Pull Requests from local
Git commits that may be amended and rebased. Pull Requests can be stacked to
allow for a series of code reviews of interdependent code.

spr is pronounced /ˈsuːpəɹ/, like the English word 'super'.

## Changes specific to `github.com/aristanetworks/cordspr`

### Breaking Changes

#### Commit Trailers

Change the names of sections added to commits, to be more like git commit trailers.
At a minimum, there cannot be whitespace within a trailer token, so replace whitespaces with
dashes (`-`).

It also appears that trailer tokens are case-sensitive, so don't accept lowercase equivalents
when "parsing" the commit message.

##### Trailer-Safety

`cspr` is now trailer-safe. In the original implementation, the message was parsed using an
_ad hoc_ logic that was not compliant with official git commit trailers syntax, as defined in
https://git-scm.com/docs/git-interpret-trailers

The original logic parsed the message in terms of "sections" starting with a pattern like
 `{key}: ...`.", and completely and silently discarded sections that were not known to `spr`.
 This caused all kinds of problems like:

- Subject lines starting with `{something>}: ...` were discarded;
- Lines in the body starting with `{something>}: ..` were discarded (e.g. `https://...`);
- All commit trailers not known to `spr` (e.g. , `Fixes: ..`) were discarded.

Also, the commit messages were re-written in a manner that was not compliant with the
official git trailers syntax, where trailers must be grouped in the last paragraph of the message,
multi-line trailers must be indented and so on.

##### Behavior Changes

We fixed the problems described above by making both the message parsing and re-writing
logic compliant with the official git trailers syntax. This however breaks with the previous behavior
in the following manner:

- The **entire** body (i.e. excluding message subject and all trailers) is now treated as what
  previously the "Summary" section.

  _Notice_: we no longer require or discard the `Summary:` section "header" (which was anyway
   strangely discarded when `spr` re-wrote the message.)

- All other known sections (e.g., `Test-Plan:`, `Reviewers:`, etc.)
  **MUST now adhere to the official trailers syntax**, which means:

  - There must be no spaces before the trailer keys;

  - They must be grouped in the last paragraph of the message. I.e., if they're not in the last
    paragraph, they're treated as part  of the summary section;

  - In multi-line trailers, the subsequent lines must be indented by at least one space;

- Trailers unknown to spr are no longer discarded when the message is rewritten.

**NOTICE**: the official trailer syntax is not very user-friendly for manual editing (e.g.
a extra blank line bettwen trailers causes git not to treat the lines before the blank
line as trailers), so it can be error prone. Be careful and make sure to double-check
the messages after using spr (e.g. after `cspr diff`).

#### Miscellaneous

- Rename executable to `cspr`.  There are several "stacked pull request" tools in existence;
change the name to avoid at least some conflicts ("c" for "cord").
- When summarizing diffs, require the user to enter `ABORT` instead of ampty string to abort.
Change message to "No description" if none is entered.

### Configuration

There are several changes and additions to the configuration settings stored in `.git/config`
in the `[spr]` section.

#### requireTestPlan

Change the default from `true` to `false`.

#### addReviewedBy

Add a configuration setting to control adding `Reviewed-By` trailers to commit messages.
Change the default from `true` to `false`.

#### autoUpdateMessage

Add a configuration setting to automatically set `--update-message` when updating changes.
The default is `true`, which makes the git commit message and title the source of truth.

#### addSprBannerComment

Add a configuration setting to enable/disable adding `[spr]` and `Created by spr X.Y.Z`
comments to generated commits.  Other comment fragments are preserved, such as `Initial commit`,
though the initial letters are uppercase, so they read slightly better without the banner text.

Change the default from `true` to `false`.

#### addSkipCiComment

Add a configuration setting to enable/disable adding `[skip ci]` to the initial generated commit
for a PR.

Change the default from `true` to `false`.

## Documentation

Comprehensive documentation is available here: https://getcord.github.io/spr/

## Installation

### Binary Installation

#### Using Homebrew

```shell
brew install spr
```

#### Using Nix

```shell
nix-channel --update && nix-env -i spr
```

#### Using Cargo

If you have Cargo installed (the Rust build tool), you can install spr by running

```shell
cargo install spr
```

### Install from Source

spr is written in Rust. You need a Rust toolchain to build from source. See [rustup.rs](https://rustup.rs) for information on how to install Rust if you have not got a Rust toolchain on your system already.

With Rust all set up, clone this repository and run `cargo build --release`. The spr binary will be in the `target/release` directory.

## Quickstart

To use spr, run `spr init` inside a local checkout of a GitHub-backed git repository. You will be asked for a GitHub PAT (Personal Access Token), which spr will use to make calls to the GitHub API in order to create and merge pull requests.

To submit a commit for pull request, run `spr diff`.

If you want to make changes to the pull request, amend your local commit (and/or rebase it) and call `spr diff` again. When updating an existing pull request, spr will ask you for a short message to describe the update.

To squash-merge an open pull request, run `spr land`.

For more information on spr commands and options, run `spr help`. For more information on a specific spr command, run `spr help <COMMAND>` (e.g. `spr help diff`).

## Contributing

Feel free to submit an issue on [GitHub](https://github.com/getcord/spr) if you have found a problem. If you can even provide a fix, please raise a pull request!

If there are larger changes or features that you would like to work on, please raise an issue on GitHub first to discuss.

### License

spr is [MIT licensed](./LICENSE).
