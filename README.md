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

1. Never use the `master` branch while developing a feature.

2. Keep commits to one idea at a time.

3. If the feature is a new parser or function, write a test for it.

4. Compile and run tests on feature.

5. Submit pull request using the branch you are working on.

[1]: http://jinja.pocoo.org/
[2]: https://github.com/sourrust/karver/issues
