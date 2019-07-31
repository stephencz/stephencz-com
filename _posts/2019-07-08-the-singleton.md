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
---

The **Singleton** design pattern is one of the easiest design patterns to understand. 
It is also one of the most controversial.
In this article, I attempt to breakdown the pattern in its entirety.

In the [What is the Singleton](/posts/the-singleton#what-is-the-singleton) section I define the pattern and explore when and why you *might* consider using it.
In the [Implementing the Singleton]() section I walk through several examples of how the pattern is commonly implemented in Java.
Finally, in [The Controversy]() section I examine some of the arguments for and against the pattern, and answer the vital question: *Should I use the Singleton?*

<!--more-->

## What is the Singleton?
The Singleton is one of twenty-three design patterns originally outlined in the famous book [*Design Patterns: Elements of Reusable Object-Oriented Software*](https://en.wikipedia.org/wiki/Design_Patterns).
The pattern is classifed as a creational pattern, meaning it is a pattern that manages the creation of objects.
Specifically, the Singleton manages the creation of a single object: Itself. 

The intent of the Singleton pattern, as originally stated, is to:

<blockquote>
<p>
Ensure a class only has one instance, and provides a global point of access to it.
<cite>{% cite GOF:1995 --file singleton  %}</cite>
</p>
</blockquote>

The full details of how this is done are covered in the [Implementing a Singleton](#implementing-a-singleton) section.
Whether or not you'd want to do it is covered in [The Controversy](#the-controversy) section.

In any case, the Singleton pattern is used when you want to limit a class to a single instance.


But why would you want to do this? 
Why would you want to limit a class to a single instance?

### A Car With Two Drivers
In our universe, when you're driving a car and want to turn, you turn in the direction you want the car to go.
If you want to go to the supermarket, and the supermarket is to the right, you turn the car right.
If you want to see a movie, and the movie theater is to the left, you turn the car left.
Your car's steering system is more or less a Singleton: There is a single instance of it, and it provides a global point of access through the steering wheel. [^1]

Now, let's imagine a universe where every car has two steering systems.
The first steering system controls the wheels on the left side of the car, while the second steering system controls the wheels on the right side of the car.
In this universe, when you're driving a car and want to make a turn, both drivers must turn in the direction they want the car to go.
If they want to go to the supermarket, both drivers must turn right.
If they want to go to the movie theater, both drivers must turn left.

But, what happens when there is a conflict of interest?
What happens when you want to go to the supermarket, but the other driver wants to go to the movie theater? 
You'll turn right, they'll turn left, and the car will skid to a stop.

<img src="https://i.imgur.com/mRF7pEW.png" class="img-fluid" alt="An image depicting the two scenarios described above. One car has one steering system and is successfully turning right. Another car has two steering systems which are turning in conflicting direction i.e. towards or away from one another.">

Because the direction the car is trying to move is shared between two steering sytems any differences between those systems have the potential to cause problems.
The Singleton design pattern offers up a fix: Limit car steering systems to a single instance.

By limiting our car's steering system to a single instance we remove the possibility for error.
The direction the car is trying to move will never conflict with itself, because the system which controls the car's direction is limited to a single instance. [^2]


### Computing Terms
In analogous terms, classes that are not limited to a single instance, but share some form of state, behavior, or access across instances, might also introduce the potential for error.

For example, consider a class that reads a configuration file.
The class first loads a configuration file stored somewhere on the computer, and then it sets its fields to match the settings of the file.
What can happen if this configuration class is instantiate more than once?

If the configuration file itself changes between the creation of instances, then the different instances might have different configuration settings.
This could lead to unpredictable behavior as different parts of the program which rely on the configuration objects might behave in contridictary ways.

This point is this:

In some situations allowing a class to be instantiated more than once can cause a program to behave unpredictably and incorrectly.
The Singleton design pattern is one solution to such situations. 
The pattern limits a class to a single instance, and provides a global point of access to that instance.


## Implementing a Singleton

We know that the Singleton pattern does two things:

{: .alpha }
1. It limits a class to a single instance.
2. And it provides a global point of access to that instance.

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

[^1]: While the Singleton pattern calls for a global point of access, a car's steering wheel is not really a global point of access. In the computing world, a global point of access is a point that can be accessed everywhere and by anything. In the physical world such points of access do not exist. In this example the word *global* is relative to the setting i.e. the car. When someone inside the car wants to access the single instance of the steering system, they do so through the steering wheel.

[^2]: Obviously, following the Singleton pattern is not the only solution to the problem of conflicting state and behavior. Software design can be as endlessly complex as we want it to be. A scenario in which a car has multiple steering systems could undoubtly be made to work without using a Singleton. However, the Singleton pattern represents *a* solution to the problem. A solution which is fairly simple and straightforward.