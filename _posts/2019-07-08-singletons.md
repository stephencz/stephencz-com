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

The goal of this article is to present an overview of the Singleton pattern. 
I begin by defining the pattern itself, and then explore why it might be useful.
I then demostrate how to implement the pattern in Java.
Finally, I delve into the controversy behind the pattern and ask the vital question: *Should I use a Singleton?*

{: .small-note }
This post belongs to [a series of posts on design patterns](/posts/design-patterns).

<!--more-->

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

The Singleton pattern is used when you want to limit a class to a single instance.
The full details of how this is done are covered in the [Implementing a Singleton]() section.
Whether or not you'd want to do it is covered in [The Controversy]() section.

In any case, the basic idea is to eliminate the ability for the class to be instantiated through a constructor, and instead have the class handle its own instantiation.
A Singleton class keeps track of a single instance of itself, and provides a global point of access to that instance.

But why would you want to do this? 
Why would you want to limit a class to a single instance?

### A Car With Two Drivers
In our universe, when you're driving a car and want to turn, you turn in the direction you want the car to go.
If you want to go to the supermarket, and the supermarket is to the right, you turn the car right.
If you want to see a movie and the movie theater is to the left, you turn the car left.
Your car's steering system is more or less a Singleton: There is a single instance of it, and it provides a global point of access through the steering wheel. [^1]

Now, let's imagine a universe where every car has two steering systems.
The first steering system controls the wheels on the left side of the car, while the second steering system controls the wheels on the right side of the car.
In this universe, when you're driving a car and want to make a turn, both drivers must turn in the direction they want the car to go.
If they want to go to the supermarket, both drivers must turn right.
If they want to go to the movie theater, both drivers must turn left.

But, what happens when there is a conflict of interest?
What happens when you want to go to the supermarket, but the other driver wants to go to the movie theater? 
You'll turn right, they'll turn left, and the car will skid to a stop.

<img src="https://i.imgur.com/mRF7pEW.png" class="img-fluid">

Because the angle of the car's wheels, and, by extension, the direction the car is moving, is shared between two steering sytems, any differences between these systems has the potential to introduce an error.
The Singleton design pattern offers up a fix: Limit car steering systems to a single instance.

By limiting our car's steering system to a single instance we remove the possibility for error.
The direction the car is trying to move will never conflict with itself.

In analogous terms, classes that are not limited to a single instance, but share some form of state or behavior across instances, might also introduce the potential for error.
The classic real-world example to demostrate this is a logger.

The purpose of a logger is to make a chronological record of a program's behavior.
Having a record of what happened during a program's execution making debugging problems easier.

Typically, a logger does this by writing important information to a log file.

Here is an example of what the log file of a video game might look like:

~~~
log-07-20-2019.txt
==========================================================================
[07-20-19 5:32:03] [INFO] Creating new window.
[07-20-19 5:32:04] [INFO] New window successfully created.
[07-20-19 5:32:03] [INFO] Initializing rendering system.
[07-20-19 5:32:03] [INFO] Rendering system successfully initialized.
[07-20-19 5:32:03] [INFO] Initializing audio system.
[07-20-19 5:32:03] [INFO] Audio system successfully initialized.
[07-20-19 5:32:03] [INFO] Loading game graphic assets.
[07-20-19 5:32:03] [INFO] Loaded graphic asset "/assets/sprites/player.png".
[07-20-19 5:32:03] [ERROR] Failed to load graphic asset "/assets/sprites/sword.png".
[07-20-19 5:32:03] [INFO] Terminating program.
==========================================================================
~~~

By reviewing this log file we can piece together an image of what went right and wrong.
We know that the window was successfully created.
We know that the rendering and audio systems were successfully initialized.
And we know that the game terminated after failing to load a graphic asset called `sword.png`.

Basic reasoning

## Implementing a Singleton

We know that the Singleton should do two things:

{: .alpha }
1. It should limit the 

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

## The Controversy




## References
{% bibliography --file singleton %}

[^1]: While the Singleton pattern calls for a global point of access, a car's steering wheel is not really a global point of access. In the computing world, a global point of access is a point that can be accessed everywhere and by anything. In the physical world such points of access do not exist. In this example the word *global* is relative to the setting i.e. the car. Anyone inside the car can access the steering system through the steering wheel.

[^2]: It is important to understand that the logger isn't strictly limited to the choices of a single instance that is globally available, or multiples instances. It is perfectly possible to have a single logger that is not globally available, but is instead passed around to where it is needed. I will talk about this later on in the article.