# Karver

Karver is a template engine written in Haskell, and the syntax is
heavily inspired by [jinja2][1].

The project is in early development, so it isn't as full featured or
production ready as jinja is. However, karver's main focus is for
Haskell programmers, wanting a template engine that is simple and has
good performance.

A small taste of the syntax being:

```
{% if title %}
<h1 id="{{ title }}">{{ title }}</h1>
{% endif %}

<ul>
{% for item in items %}
  <li>{{ item }}</li>
{% endfor %}
</ul>
```

# Interface of Karver

The meat of karver is the `renderTemplate` function. With it's type
signature being, `renderTemplate :: HashMap Text Value -> Text -> Text`,
it takes a hashmap of variables being used by the template, and the
template text itself. It of course returns the translated result, and
since the type is `Text`, it supports Unicode right out of the box.

Programs using karver might look something like:

```haskell
import Text.Karver
import qualified Data.HashMap.Strict as H
import qualified Data.Text.IO as T
import qualified Data.Vector as V

templateHashMap :: HashMap Text Value
templateHashMap = H.fromList $
  [ ("title", Literal "Grocery List")
  , ("items", List $ V.fromList
                ["eggs", "flour", "cereal"])
  ]

main :: IO ()
main = do
  tplStr <- T.readFile "path/to/template.html"
  let htmlStr = renderTemplate templateHashMap tplStr
  T.writeFile "path/to/output.html"
```

# How to Contribute

Since karver is in early development, there is still work needed to be
done. Whenever there is something that needs work on, it will typically
be in [issues][2]. If there isn't issues open, try working on a feature
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
tpope's, guide][3]. And if you want to make things easier on yourself,
use a client for working with commit message. For me personally, I use
[fugitive][4] because my editor of choice is vim.

### If the feature is a new parser or function, write a test for it.

I don't care whether you write test first and then add the
implementation, or vice-versa. As long as there is some kind of test,
that will show that the function is working correctly, I am happy.

### Compile and run tests on feature.

This the just so I know, at the very least, the code is working for you
computer and you didn't break any of the other tests.

### Submit pull request using the branch you are working on.

Simple as that.

## Getting Started

```bash
git clone git://github.com/sourrust/karver.git
cd karver
cabal configure --enable-tests
```

If the configure set fails you are going to want to install the missing
packages and try again. Karver is built on the [latest Haskell
Platform][5] and a few other dependencies.

```bash
cabal update
cabal install attoparsec \
              hspec \
              unordered-containers
```

And you're pretty much good to go. Just re-configure and `cabal build`
and `cabal test` to run the test suite.

## Writing Tests

Karver uses [`hspec`][6] for testing. Tests are located in the `test/`
directory and each file, being tested, has it's own corresponding Spec
file. For example, `Text/Karver/Parser.hs` in `src/`, has a spec file
`Text/Karver/ParserSpec.hs` inside of `test/`. Follow this rule if you
add a new file that you want to test, because [`Spec.hs`][7] discovers
the files with the name, so it needs Spec prefixing the file name for
hspec to add it to the suite.

Now, actually writing the test is pretty simple.

```haskell
describe "function you are testing" $ do
  it "case you will test for" $ do
    let value    = -- result from the function you are testing
        expected = -- what you expect the value to be
    value `shouldBe` expected
```

You can add more variable if needed, but the **value should be
expected**, is just my personal preference to how the test should end.

[1]: http://jinja.pocoo.org/
[2]: https://github.com/sourrust/karver/issues
[3]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[4]: https://github.com/tpope/vim-fugitive
[5]: http://www.haskell.org/platform/
[6]: http://hspec.github.io/
[7]: https://github.com/sourrust/karver/blob/master/test/Spec.hs
