When I ask you to run cargo check, please use:
 `cargo check --all-features --tests`
When I ask you to run clippy, you should always use:
`cargo clippy  --fix --allow-no-vcs`

# jj

I use jj for version control, and I've added some aliases I'd like you do use.

Generally, I'd prefer if you could always make substantial changes in a fresh revision.
You should always first check the current stack of revisions using `jj stack`,
and make a new one using one of:
* add before a revision `jj new -B <parent revision>`
* add after a revision `jj new -A <parent revision>`

If you want to read the diff for a revision, always use `jj gdiff -r <revision>`; this will format it in git format.

If you have to use git diff, make sure to pass the `--no-ext-diff` flag to
format the diff properly for you

# grep
Grep is fine to use, but if you're trying to do any advanced recursive greps,
you should probably use ripgrep (since it's much faster)

