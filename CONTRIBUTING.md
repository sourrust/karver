# How to Contribute

Since karver is in early development, there is still work needed to be
done. Whenever there is something that needs work on, it will typically
be in [issues][1]. If there isn't issues open, try working on a feature
that is in jinja, or an other template engines, that you want to see in
karver.

And before you start hacking on karver, here a same guide to go from add
code to the project and getting it into the main repo.

### Never use the `master` branch while developing a feature.

When writing a new feature use a branch name that describes what you are
working one, e.g. `parse/variable-assignment` if were working on a
parser for variable assignment.

The `master` branch is mainly for merging and some time version bumping;
but, most of the time, you are never going to touch it directly.

### Keep commits to one idea at a time.

Committing small changes is the best way, for me at least, to look at
what has changed during development. It is almost like a self documented
changelog in the commit message.

For a easy guide on how to structure you commit message just [follow
tpope's, guide][2]. And if you want to make things easier on yourself,
use a client for working with commit message. For me personally, I use
[fugitive][3] because my editor of choice is vim.

### If the feature is a new parser or function, write a test for it.

I don't care whether you write test first and then add the
implementation, or vice-versa. As long as there is some kind of test,
that will show that the function is working correctly, I am happy.

### Compile and run tests on feature.

This the just so I know, at the very least, the code is working for you
computer and you didn't break any of the other tests.

### Submit pull request using the branch you are working on.

Simple as that.

[1]: https://github.com/sourrust/karver/issues
[2]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[3]: https://github.com/tpope/vim-fugitive
