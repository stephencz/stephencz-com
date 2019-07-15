---
layout: post
title: The Singleton
description: >- 
  Singletons, while one of easiest design patterns to understand, have a controversial existence.
  Some people hate them, while others love them. In this article, I define what a Singleton is,
  demostrate common implementations of the pattern, and explore the patterns love-hate relationship with programmers.
author: Stephen Czekalski
categories: [design patterns]
updated: 
preqs: 
parts:
excerpt_separator: <!--more-->
---

<div class="sidenote-right sidenote-toc">

</div>

The **Singleton** design pattern is among the easiest design patterns to understand. It is also one of the most controversial.

In this article, I present an overview of the Singleton.
I begin by defining it and outlining its common usage.
I then provide several examples of how a Singleton can be implemented in Java.
And, finally, I explore the controversy behind the pattern and answer the vital question: *Should I use a Singleton?*

<!--more-->

## What is a Singleton?
The Singleton is one of twenty-three design patterns originally outlined in the famous book: [*Design Patterns: Elements of Reusable Object-Oriented Software*](https://en.wikipedia.org/wiki/Design_Patterns).
The pattern is classifed as a creational pattern, meaning it is a pattern that manages the creation of objects.

Specifically, the Singleton manages the creation of a single object: Itself. 
The intent of the Singleton pattern, as originally stated, is to:

>  Ensure a class only has one instance, and provide a global point of access to it.

This is as concise a definition you will ever need for the Singleton.
Just remember that a Singleton does two things:

{: .alpha }
1. It ensures that a class has only a single instance.
2. It provides a global point of access to that instance.

That's it.

### When and Why Would You Use A Singleton?
There are people that argue that you should never use a Singleton, and peop

## Should I Use a Singleton?
The short answer to that quesion is... when it makes sense to use a Singleton.


## Sources

* [A reason why Singletons can be bad](https://cocoacasts.com/are-singletons-bad)
* [#Static class vs Singleton](https://softwareengineering.stackexchange.com/questions/235527/when-to-use-a-singleton-and-when-to-use-a-static-class) 