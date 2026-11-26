# GitHub and SSH

Musicomp is part of the parent repository at `/home/mcarroll/Documents/prolog/prolog`. Run Git commands here or in that parent directory; both operate on the same repository. Its `origin` is `git@github.com:musicarroll/prolog.git`.

Your SSH configuration for `github.com` uses `~/.ssh/archive/id_ed25519`, the same key as the `github-wwdev` alias used by the Wordwalk website. The `github-symmetry` alias uses a different key, `~/.ssh/archive/symmetry_monograph_ed25519`. The parent repository's existing SSH remote was successfully checked with `git ls-remote`; its configuration does not need to change.

To review and publish changes in the usual way:

```sh
cd /home/mcarroll/Documents/prolog/prolog
git status
git add musicomp/README.md musicomp/DEVELOPMENT.md musicomp/GITHUB.md musicomp/.gitignore
git diff --cached
git commit -m 'Document Musicomp'
git push origin main
```

Other changes may already exist in the parent repository; review what is staged before committing. The parent's ignore rules exclude some Musicomp score and export formats; inspect those rules before deciding whether to track any of those files. A GitHub authentication check can be run with `ssh -T git@github.com`; a successful greeting normally exits with status 1 because GitHub does not offer shell access.
