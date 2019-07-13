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
parts:
preqs: 
excerpt_separator: <!--more-->
---

The **Singleton** design pattern is among the easiest design patterns to understand.
It is also one of the most controversial.

In this article, I present an overview of the Singleton.
I begin by defining it and outlining its common usage.
I then provide several examples of how a Singleton can be implemented in Java.
And, finally, I explore the controversy behind the pattern and answer the vital question: *Should I use a Singleton?*

<!--more-->

## What is a Singleton?

The Singleton is one of twenty-three design patterns originally outlined in the famous book: [*Design Patterns: Elements of Reusable Object-Oriented Software*](https://en.wikipedia.org/wiki/Design_Patterns).
The pattern is classifed as a creational pattern, meaning it is a pattern that manages the creation of objects.

Specifically, the Singleton manages the creation of itself. 
The intent behind the pattern was originally defined as:

>  Ensure a class only has one instance, and provide a global point of access to it.

This is as concise a definition you will find.
A Singleton does two things. Its ensures that a class, typically itself, has only one instance.
And it provides a global point of access to that instance.

### When and Why?