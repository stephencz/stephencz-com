---
layout: page
title: About
description: >-
  Information about Stephen Czekalski and this website.
toc: true
---

## Me

## Site


### Jekyll

<!-- Declaration of my use of Jekyll -->
This website was built with the static site generator [Jekyll](https://jekyllrb.com/).

#### Why Jekyll?

Before learning about Jekyll, I used the server-sided content management system [WordPress](https://wordpress.com/).

WordPress is a fine tool, but only if you actually need its features and have the power to support them.
I'm the only person who writes for this website, so I don't need a massive platform that supports multiple authors.
I don't need access to a store with ten thousand different themes and plugins.
I don't need a comment section. 
I don't need a media library.
And need to support multiple types of content.

{% sidenote left collapsible center width-300 %}
**Did you know:** Prior to switching to Jekyll, this website regularly had load times of over 10 seconds? 
{% endsidenote %}

I want my website to be fast, and I want it at a low price.
This is why I switched to Jekyll.

The issue isn't a Jekyll vs. WordPress issue, but is a [static web page](https://en.wikipedia.org/wiki/Static_web_page) vs [dynamic web page](https://en.wikipedia.org/wiki/Dynamic_web_page) issue:

* Static web pages are served to your web browser as they are stored.
* Dynamic web pages are stitched together from independent parts by server-sided software as they are requested.

Static web pages are fixed (except for what can be changed with client side languages such as Javascript).
Whereas dynamic web pages are... dynamic. 
The use of server sided languages, PHP in the case of WordPress, gives them their flexibility.

{% sidenote right collapsible center width-300 %}
**Did you know:** The bare-bones size of this website is less than 300KB? WordPress Core is roughly 30MB.
{% endsidenote %}

A WordPress theme is divided by its distinct parts.
There is a file containing only the header. 
There is a file containing only the navigation.
There is a file containing the place where the page's content goes.
There is a file containing the sidebar.
There is a file for the footer. 
And on it goes.

The *dynamic* in *dynamic web page* comes from WordPress's ability to stitch these individual parts together on the fly.
Making more or new forms of content becomes easy. And making site-wide changes becomes trivial.
The cost of this ease is the backend processing time needed to create and serve the dynamic web pages as they are requested.

A static web page is little more than HTML, CSS, and perhaps some Javascript.
There is no server-side action going on.
The page exists on the server and, when it is requested, it is served as it exists.

Before tools like Jekyll, managing anything but a small static website was unrealistic.
Can you imagine the headache that comes with making site-wide changes when you don't have the ability to dynamically inject content where it is needed?
Say you want to change the site's navigation. 
You would have to go to each separate static page, and makes the change manually.
This was far from ideal.
The benefit being that static web pages require little backend processing, and are thus able to be served faster.

Jekyll provides the best of both worlds. 
It is effectively a dynamic website preprocessor.
Through the combined use of various technologies, Ruby, the Liquid templating language, YAML, HTML, CSS/SASS, and so on, Jekyll is able to provide a dynamic-website-like content management system while
retaining the benefit of a static website: Speed.

As someone who wants those two things, and wants them cheap, Jekyll is an ideal solution.

#### Source Code

The source code for this website can be found [here](https://github.com/stephencz/stephencz-com).

#### Plugins and Scripts
