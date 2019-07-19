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

The **Singleton** design pattern is among the easiest design patterns to understand. 
It is also among the most controversial.

In this article, I present an overview of the Singleton.
I begin by defining it, and outlining a common use case.
I then cover how it is implemented in Java.
And, finally, I explore the controversy behind the pattern and answer the vital question: *Should I use a Singleton?*

<!--more-->

## What is a Singleton?

The Singleton is one of twenty-three design patterns originally outlined in the famous book: [*Design Patterns: Elements of Reusable Object-Oriented Software*](https://en.wikipedia.org/wiki/Design_Patterns).
The pattern is classifed as a creational pattern, meaning it is a pattern that manages the creation of objects.

Specifically, the Singleton manages the creation of a single object: Itself. 
The intent of the Singleton pattern, as originally stated, is to:

>  Ensure a class only has one instance, and provides a global point of access to it.

Traditionally, 

This is as concise a definition you will ever find for the Singleton.
Just remember that a Singleton does two things:

{: .alpha }
1. It ensures that a class has only a single instance.
2. It provides a global point of access to that instance.

That's it.

<div class="sidenote-column">
</div>

## Implementing a Singleton

The classic use case for a Singleton is logging.
When building an application it is often useful to have information about the status and behavior of its systems outputted in some way.
For simple programs outputting a couple lines to a console might help debug a problem, but as applications grow in complexity they require more complete solutions.
A dedicated logging system is often that solution. 

To learn the Singleton pattern, I will walk you through the implementation of a rudimentary logger class in Java.
Let's begin by outlining what we want our logger to do, and what the Singleton pattern requires.

Our logger is going to be simple.
It will be a single method which takes a `String` as a parameter, and outputs that string to the console.

~~~java
public class Logger {

    public void log(String message) {
        System.out.println(message);
    }

}
~~~



~~~java
public class Logger {

    private static final Logger INSTANCE = new Logger();

    private Logger() { }

    public void log(String message) {
        System.out.println(message);
    }

    public static Logger getInstance() {
        return INSTANCE;
    }
}
~~~

There are three parts to this implementation: The `INSTANCE` field, a private constructor, and the public static method `getInstance()`.

~~~java
    private static final Singleton INSTANCE = new Singleton();
~~~

The `INSTANCE` field holds the single instance of our Singleton.
By declaring it as `private` and `static` we transfer ownership to the class itself while protecting it from outside access.
We also declare as final, and instantiate a new `Singleton` object immediately.



~~~java
Logger logger = logger.getInstance();
logger.log("Hello, World!");
~~~


### Eager vs. Lazy Initialization

~~~java
public class LazyLogger {

    private static LazyLogger instance = null;

    private LazyLogger() { }

    public void log() { ... }

    public static LazyLogger getInstance() {
        if(instance == null) {
            instance = new LazyLogger();
        }

        return instance;
    }
}
~~~

~~~java
public class Singleton {
    
    private Singleton() { }
    
    private static class Holder {
        static final HolderSingleton INSTANCE = new Singleton();
    }
    
    public static Singleton getInstance() {
        return Holder.INSTANCE;
    }
    
}
~~~

## The Controversy
We know that a Singleton does two things: It limits a class to a single instance, and it provides a global point of access to that instance.
But why *might* that be useful?

The classic use case for a Singleton is logging.
When building an application it is often useful to have information about the status and behavior of its systems outputted in some way.
For simple programs outputting a couple lines to a console might help debug a problem, but as applications grow in complexity they require more complete solutions.
Widespread logging is often that solution.

<blockquote class="cited-quote">
<p>The classic use case for a Singleton is logging.
When building an application it is often useful to have information about the status and behavior of its systems outputted in some way.
For simple programs outputting a couple lines to a console might help debug a problem, but as applications grow in complexity they require more complete solutions.
Widespread logging is often that solution. <cite>{% cite Densmore:2004 --file singleton %}</cite>
</p>
</blockquote>

Put yourself in the shoes of someone who has to implement a logger.
Consider some of your options:

* You could create an instance of your logger class as a global variable, and access it where you need it.
* You could create a new instance of your logger class everywhere it is needed.
* You could create a single instance of your Logger class and pass it down a chain of constructors to every class that needs it.
* You could create a static class.

In the case of a logger none of these solutions are great. {% cite --file singleton Densmore:2004 %}

## Should I Use a Singleton?

The short answer to that question is: *Maybe*.


## References
{% bibliography --file singleton %}