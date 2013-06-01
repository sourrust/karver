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

[1]: http://jinja.pocoo.org/
