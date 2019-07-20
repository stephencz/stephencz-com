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
reading_time:
excerpt_separator: <!--more-->
---

The **Singleton** design pattern is among the easiest design patterns to understand. 
It is also among the most controversial.

In this article, I present an overview of the Singleton.
I begin by defining it, and outlining a common use case.
I then cover how it is implemented in Java.
And, finally, I explore the controversy behind the pattern and answer the vital question: *Should I use a Singleton?*
<!--more-->

{: .small-note }
This post belongs to [a series of posts on design patterns]().

## What is a Singleton?
The Singleton is one of twenty-three design patterns originally outlined in the famous book: [*Design Patterns: Elements of Reusable Object-Oriented Software*](https://en.wikipedia.org/wiki/Design_Patterns).
The pattern is classifed as a creational pattern, meaning it is a pattern that manages the creation of objects.

Specifically, the Singleton manages the creation of a single object: Itself. 
The intent of the Singleton pattern, as originally stated, is to:

<blockquote>
<p>
Ensure a class only has one instance, and provides a global point of access to it.
<cite>{% cite GOF:1995 --file singleton  %}</cite>
</p>
</blockquote>

This is as concise a definition you will ever find for the Singleton.
Just remember that the pattern does two things:

{: .alpha }
1. It ensures that a class has only a single instance.
2. It provides a global point of access to that instance.

But what does that mean and why *might* that be useful?

### A Car With Two Steering Systems
In our universe, when you're driving a car and want to turn, you turn in the direction you want the car to go.
If the supermarket is to the right, you turn right.
If you want to see a movie and the movie theater is to the left, you turn left.
Your car's steering system is more or less a Singleton: There is a single instance of it, and it provides a global point of access through the steering wheel. [^1]

Now, let's imagine a universe where every car has two steering systems and two steering wheels.
In this universe, when you're driving a car and want to turn, both drivers must turn in the direction they want the car to go.
If you want to go to the supermarket, both drivers must turn right.
If you want to go to the movie theater, both drivers must turn left.

What happens when there is a conflict of interest?
What happens when you want to go to the supermarket, but the other driver wants to go to the movie theater? 
You'll turn right, they'll turn left, and the car will probably crash.

Because the the cars direction is shared between two steering sytems, any differences between these systems has the potential to introduce an error. [^2] 
The Singleton design pattern offers up a fix: Limit your car to a single steering system. 

### Logger Example
In analogous terms, classes that are not limited to a single instance, but share some form of state or behavior, might also have the potential for error.
The classic example of such a situation is a Logger.

The purpose of a Logger is to record information about a program's runtime.
This is typically done by writing important information to a text file
Here is what the content of a video game's log file might look like:

~~~plaintext
log.txt
===================================================================================
[July 20, 2019 10:43:32 AM] [INFO] Initializing rendering system.
[July 20, 2019 10:43:35 AM] [INFO] Rendering system successfully initialized.
[July 20, 2019 10:43:37 AM] [INFO] Initializing audio system.
[July 20, 2019 10:43:40 AM] [INFO] Audio system successfully initialized.
[July 20, 2019 10:43:41 AM] [INFO] Creating window.
[July 20, 2019 10:43:42 AM] [INFO] Window successfully created.
[July 20, 2019 10:43:43 AM] [INFO] Loading graphic assets.
[July 20, 2019 10:43:48 AM] [ERROR] Failed to load sword.png.
[July 20, 2019 10:43:50 AM] [INFO] Terminating execution.
===================================================================================
~~~

By reading the logger's output we can piece together went right and wrong during the video game's execution.
We know that the rendering and audio systems were successfully initalized. 
We know that a window was successfully created.
And we also know that the graphic asset `sword.png`  failed to load.

Rendering and audio system initialization?
Window creation?
Graphic asset loading?
I'm not sure what any of that means, but what they show us is that this program's logging facilities are widely available.

In this example let's assume that our logging facilities are provided by a class called `Logger`.
How can such a class 

When they try to access the same file unsuccessfuly. And successfully overwriting each other. 

Starting at different times. etc.


### The Singleton Solution

## Implementing a Singleton


~~~java
public class Singleton {

    private static final Singleton INSTANCE = new Singleton();

    private Singleton() { }

    public static Singleton getInstance() {
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



## References
{% bibliography --file singleton %}

[^1]: While the Singleton pattern calls for a global point of access, a car's steering wheel is not really a global point of access. In the computing world, a global point of access is a point that can be accessed everywhere and by anything. In the physical world such points of access do not exist. In this example the word *global* is relative to the setting i.e. the car. Anyone inside the car can access the steering system through the steering wheel. Accordingly, the steering wheel is the global point of access within our setting.

[^2]: This is assuming that the correct behavior of a car in a universe where every car has two steering systems is to drive like our cars do.