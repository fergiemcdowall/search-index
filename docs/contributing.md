# Contributing

## Guiding principles

### Be bold

`search-index` is run as gatekeepered wiki. Any reasonable and useful new features will be accepted, unless there is a general consensus not to. This is a do-ocracy, so be be bold- as a casual user of `search-index` you are entitled to make any improvements that you see fit.

### Be nice

Be nice to your fellow hackers, that way this project can be [psychologically safe](https://en.wikipedia.org/wiki/Psychological_safety), which leads to better user and developer experiences.

### Be platform agnostic

The development environment that you use is clearly superior in many ways. However other contributors are lamentably still stuck with lesser environments, and therefore all config needs to work on their stuff as well. Use npm and node standards, and things should generally work as intended.

### Be mindful of web browsers

`search-index` should work in web browsers, and be as small as possible. Avoid large dependencies and syntax that is not supported on all recent (last 2 years) browsers.

### Take credit

If you make a larger contribution, please add yourself as a contributer.

## Submitting patches and improvements

### No change is too small...

Often the small changes are the best ones. If you see a little "easy" improvement, the project would love to get that pull request. People have have different expertise and focus: what seems insignificant, obvious and easy to you, can be elegant and insightful to others.

### ...especially in the documentation (grammer nazis welcome)

Technical documentation is prone to typos, broken links, and poor formulation, which all undermine the quality of the project. Good documentation is key for increasing adoption and improving developer experience. Pull requests incorporating improvements to the documentation (no matter how small) are greatly appreciated.

### Use Semver

Semver is good. Use it.

### Write commit messages in semantic-release format

This project uses [`semantic-release`](https://github.com/semantic-release/semantic-release) to keep the code-base tagged and versions correctly assigned.

When commiting, please use semantic-release's [default commit message format](https://github.com/semantic-release/semantic-release#default-commit-message-format). That way, your contributions will appear in the [changelog](https://github.com/fergiemcdowall/search-index/releases)
