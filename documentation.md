---
layout: page
title: Site Documentation
description: >-
  A full collection of the posts and post categories that can be found on this website.
toc: true
---

Oi! You there! What are you doing here? How did you get here? I don't remember making any links to this page... I'm tempted to make you leave, but you can stay... for now. Huh? What's that? What is this page? 

This page is where I keep documentation on important features of this website. How to 

## Style Guidelines

## Plugins

### Table of Contents

### Sidenotes
I created sidenotes because I wanted to have the option to display content to the left and right of the center column.



Behind the scenes sidenotes are Liquid blocks, so they can contain any valid HTML. 
Declaring a sidenote is straightforward. 

{% raw %}
```plaintext
{% sidenote <direction> <additional css classes> %}
   HTML goes here!
{% endsidenote %}
```
{% endraw %}


Sidenotes are only displayed when the width of the browser window exceeds `1500px`. 
But sidenotes given the additional class `collapsible` will merge with the main column.
A sidenote has one of two directions: **left** or **right**.
The additional CSS classes can be any defined class. 

For minor stylistic changes, CSS rules can be created and applied on the fly. For example: 

{: style="color: green;"}
{% sidenote left %}
  <p>Look at me I'm so green!</p>
{% endsidenote %}

{% raw %}
```plaintext
{: style="color: green;"}
{% sidenote left %}
  <p>Look at me I'm so green!</p>
{% endsidenote %}
```
{% endraw %}

#### collapsible

The collapsible class creates a sidenote which collapses into the main column when the browser width goes below
`1500px`.

{% raw %}
```plaintext
{: style="color: green;"}
{% sidenote left %}
  <p>Look at me I'm so green!</p>
{% endsidenote %}
```
{% endraw %}

#### Considerations

There are a few things to consider when using sidenotes. The first and most important thing to consider
is how a particular sidenote will interact with other elements of the page. What happens when a sidenote
is placed just before a contraster or a breakout? 

There are a few safe guards in place for these situations.

#### collapsible

{% contraster dark-theme %}
### Contrasters
I created contrasters because I wanted to break up the visual monotony of long posts.
Contrasters work by applying a CSS class to every element in between the starting contraster tag and the ending tag.

While a contraster can be used to apply any CSS class to its contents, they were meant to be use with a theme class.
For example, the theme class used with this contraster is called ```dark-theme```.

Here is the Liquid code for using a contraster:

{% raw %}
```plaintext
{% contraster <theme-name> %}
// Everything that should have the theme applied to it.
{% endcontraster %}
```
{% endraw %}
{% endcontraster %}

### Utility classes