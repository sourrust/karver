# Karver

[![Build Status](https://travis-ci.org/sourrust/karver.svg?branch=master)](https://travis-ci.org/sourrust/karver)

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
  <li>{{ item.name }} for {{ item.price }}</li>
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
{-# LANGUAGE OverloadedStrings #-}

import Text.Karver
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Vector as V

templateHashMap :: HashMap Text Value
templateHashMap = H.fromList $
  [ ("title", Literal "Grocery List")
  , ("items", List $ V.fromList [ Literal "eggs"
                                , Literal "flour"
                                , Literal "cereal"
                                ])
  ]

main :: IO ()
main = do
  tplStr <- T.readFile "path/to/template.html"
  let htmlStr = renderTemplate templateHashMap tplStr
  T.writeFile "path/to/output.html" htmlStr
```

or if JSON is more your flavor:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.Karver
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TI

templateHashMap :: Text
templateHashMap = T.concat $
  [ "{ \"title\": \"Grocery List\""
  , ", \"items\": [ \"eggs\", \"flour\", \"cereal\" ]"
  , "}"
  ]

main :: IO ()
main = do
  tplStr <- TI.readFile "path/to/template.html"
  let htmlStr = renderTemplate' templateHashMap tplStr
  TI.writeFile "path/to/output.html" htmlStr
```

## Getting Started

```bash
git clone git://github.com/sourrust/karver.git
cd karver
cabal configure --enable-tests
```

If the configure set fails you are going to want to install the missing
packages and try again. Karver is built on the [latest Haskell
Platform][2] and a few other dependencies.

```bash
cabal update
cabal install attoparsec \
              hspec \
              unordered-containers
```

And you're pretty much good to go. Just re-configure and `cabal build`
and `cabal test` to run the test suite.

## Writing Tests

Karver uses [`hspec`][3] for testing. Tests are located in the `test/`
directory and each file, being tested, has it's own corresponding Spec
file. For example, `Text/Karver/Parser.hs` in `src/`, has a spec file
`Text/Karver/ParserSpec.hs` inside of `test/`. Follow this rule if you
add a new file that you want to test, because [`Spec.hs`][4] discovers
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
[2]: http://www.haskell.org/platform/
[3]: http://hspec.github.io/
[4]: https://github.com/sourrust/karver/blob/master/test/Spec.hs
