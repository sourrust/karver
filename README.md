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

[1]: http://jinja.pocoo.org/
