---
layout: page
title: Docs
---

this is a 

{% for doc in site.docs %}
<a href="{{ doc.url }}">{{ doc.title }} </a>
{% endfor %}

