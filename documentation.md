---
layout: page
title: Site Documentation
description: >-
  A full collection of the posts and post categories that can be found on this website.
toc: true
---


This is a test. This is a test. This is a test. This is a test. This is a test. This is a test. This is a test. This is a test. 
This is a test. This is a test. This is a test. This is a test. This is a test. This is a test. This is a test. This is a test. 
This is a test. This is a test. This is a test. This is a test. This is a test. This is a test. This is a test. This is a test. This is a test. This is a test. 
This is a test. This is a test. This is a test. This is a test. This is a test. This is a test. This is a test. This is a test. This is a test. This is a test. 

## Breakouts

Breakouts are used to let content escape the main column. For example:

{% breakout %}

<p>Check out this ugly sentence. Why is it all the way over here? Because its inside a breakout! Seriously, inspect the source code. Are you inspecting it yet? How about now? Are you doing it now? Pssst! It doesn't actually matter what I'm saying here. This is just an example. Are you still reading this? You are? Okay... well... move on now.</p>

{% endbreakout %}

You see how the paragraph no longer respects the boundary of the main column, but still respects the flow?
That is the entire point of breakouts.
They may seem simple and useless upon first glance, but they are one of the most powerful tools I have.

A breakout can contain any valid HTML. Any. This means that they can be used to display tables, images, javascript programs, etc.

Here is the syntax for a breakout: 

{% raw %}
```plaintext
{% breakout <additional classes> %}
  //Content
{% endbreakout %}
```
{% endraw %}


## Banners

Banners are a special type of breakout.
They are used to display images that are the full width of the browser window.

{% banner /assets/img/banner-example.gif %}

That is a breakout.

{% raw %}
```plaintext
{% banner <image/url> <additional classes> %}
```
{% endraw %}

## Sidenotes

{% contraster dark-theme spacing-60 %}

## Contrasters

I create contrasters to break up the visual monotony of long posts.
Contrasters work by applying a theme, represented by a series of CSS rules, to every element within a section of a post.

Normal Liquid block statements have a starting and ending tag.
For example:

{% raw %}
```
{% block %}
  //Some HTML or Markdown
{% endblock %}
```
{% endraw %}

While the syntax looks almost identical to a block, contrasters are not blocks.
They are a kind of pseudo Liquid block.

{% raw %}
```
{% contraster <theme> <additional css> %}
  //Anything inside the contraster.
{% endcontraster <additional css> %}
```
{% endraw %}

Instead of the `contraster` and `endcontraster` tags representing the start and end of a block, they represent two different Liquid tags.
A rendered document using contrasters looks like this:

```html
<div class="contraster-start-marker dark-theme spacing-60"></div>
<!-- Everything between the contraster and endcontraster tags -->
<div class="contraster-end-marker spacing-60"></div>
```

What the `contraster` and `endcontraster` tags are doing is inserting special marker elements into the document, and acting as carriers for a theme.

The key thing to understand here is that contrasters do not actually encapsulate any content of the page. 
They merely signify where a contraster should start and where it should end.

The rest of the plugin's functionality is handled by a JQuery script. 
For each pair of `contraster-start-marker` and `contraster-end-marker` tags a new `contraster` element is creating in the body of the HTML document.

The script is then responsible for adjusting the size and position of this new contraster element when the browser is resized.
Additionally, it applies the theme supplied to the contraster Liquid tag to every element in between the start marker and end marker.

Contrasters will apply their theme to sidenotes that exist within the bounds of the contraster.
However, keep in mind that contrasters do nothing to prevent sidenotes from spilling into or out of them.
This can lead to some ugly clashing of themes.

One solution is to apply one of the `.height-xxx` classes to sidenotes in danger of spilling into or out of a contraster. 
See sidenote's [Additional Classes]() for more information.

### Themes

Currently there is one theme for contrasters: `dark-theme`.

### Additional Classes

While contrasters can accept any number of additional CSS classes, there is only one series of additional classes
made specifically for contrasters. These classes take the form of `.spacing-xxx`. 

The standard spacing of a contraster is `120px`. 
This value is divided in half and applied to the element before the contraster, the first element of the contraster, the last element of the contraster, and the first element after the contraster ends.

In other words, there is a 60px margin at the top and bottom of the contraster. A 60px bottom margin on the first element before the contraster. And a 60px margin on first element after the contraster.
This leads to consistent spacing.

{: .table style="margin-left: auto; margin-right: auto; margin-top: 30px; margin-bottom: 30px; "}
| Rule           | Effect                            |
|----------------|-----------------------------------|
| `.spacing-10`  | Sets a total margin of 10px.      |
| `.spacing-20`  | Sets a total margin of 20px.      |
| `.spacing-30`  | Sets a total margin of 30px.      |
| `.spacing-40`  | Sets a total margin of 40px.      |
| `.spacing-50`  | Sets a total margin of 50px.      |
| `.spacing-60`  | Sets a total margin of 60px.      |
| `.spacing-70`  | Sets a total margin of 70px.      |
| `.spacing-80`  | Sets a total margin of 80px.      |
| `.spacing-90`  | Sets a total margin of 90px.      |
| `.spacing-100` | Sets a total margin of 100px.     |
| `.spacing-110` | Sets a total margin of 110px.     |
| `.spacing-120` | Sets a total margin of 120px.     |

{% endcontraster spacing-60 %}