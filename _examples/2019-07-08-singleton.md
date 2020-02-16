---
layout: post
title: An Overview of the Singleton
description: >- 
  The Singleton is one of the easiest, and most controversial, design patterns to understand. 
  In this article, I attempt to breakdown the pattern in its entirety.
categories: [design patterns]
related_links: ["https://stackoverflow.com/questions/46208588/jekyll-check-if-there-are-no-posts"]
---
The Singleton design pattern is one of the easiest design patterns to understand. 
It is also one of the most controversial.

The goal of this article is to present a general overview of the pattern.
In the [What is the Singleton](/posts/the-singleton#what-is-the-singleton) section I define the pattern and explore when and why you *might* consider using it.
In the [Implementing the Singleton](#implementing-the-singleton) section I walk through several examples of how the pattern is commonly implemented in Java.
Finally, in [The Controversy](#the-controversy) section, I examine some of the arguments for and against the pattern, and answer the vital question: *Should I use the Singleton?*

<!--more-->

## What is the Singleton?
The Singleton is one of twenty-three [design patterns](https://en.wikipedia.org/wiki/Software_design_pattern){:target="_blank"} originally outlined in the famous book [*Design Patterns: Elements of Reusable Object-Oriented Software*](https://en.wikipedia.org/wiki/Design_Patterns){:target="_blank"}.

The purpose of a design pattern is to act as a reuseable solution to some commonly occuring problem.
The problem the Singleton pattern attempts to solve is that of limiting a class to a single instance.

Object-Oriented Programming is built around the ideas of the class and the object.
At a very high-level, a **class** is more or less the template of an idea. 
And an **object** is the literal manifestation of that idea in bits and bytes.

In the majority of cases, classes are design to be reused.
When simulating an ocean we want it to populated with many fish, not just one.

The intent of the Singleton pattern, as original stated by the book, is to:


{% controlledbreakout 500px %}
<blockquote>
<p>
Ensure a class only has one instance, and provides a global point of access to it.
<cite>{% cite GOF:1995 --file singleton  %}</cite>
</p>
</blockquote>
{% endcontrolledbreakout %}

{: .key}
The Singleton pattern is used when you want to limit a class to a single instance.
Put another way, it is used when you only ever want there to be *exactly* one of something.


### A Car With Two! !@$#$# Drivers!@#

In our universe, when you're driving a car and want to turn, you turn in the direction you want the car to go.
If you want to go to the supermarket, and the supermarket is to the right, you turn the car right.
If you want to see a movie, and the movie theater is to the left, you turn the car left.

Our cars, more or less, utilize the Singleton pattern.
We limit them to a single instance of a steering system, and that steering system is globally accessible through the steering wheel. [^1]

{% controlledbreakout 500px %}
This is a test.
<p>This is another test</p>

<iframe width="560" height="315" src="https://www.youtube.com/embed/-NA4CKvX6Sg" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

{% endcontrolledbreakout %}

Now, let's imagine a universe where every car has two steering systems.
The first steering system controls the wheels on the left side of the car, while the second steering system controls the wheels on the right side of the car.
To drive a car in this universe you require two drivers, each controlling a side of the car.

When they want to make a turn, both drivers must turn in the direction they want the car to go.
If they want to go to the supermarket, both drivers must turn right.
If they want to go to the movie theater, both drivers must turn left.

What happens when there is a conflict of interest?
What happens when you want to go to the supermarket, but the other driver wants to go to the movie theater? 


You'll turn right, they'll turn left, and the car will skid to a stop or crash.


Situations such as these, where having more than one of something gives rise to a conflict, are prime examples of when using the Singleton pattern *might* make sense.


The purpose of the Singleton pattern is to limit a class to a single instance.
We use it when we want there to only ever be one of something.
And we use it to protect against against conflicts of state and behavior.



### In Analogous Terms
In analogous terms, a class that is not limited to a single instance, but shares some form of state or behavior across instances, might also introduce the potential for error.
The examples where this might happen in software design are endless.

One of the classic examples is a logging system.
The purpose of logging is to make a record of a program's runtime behavior.
This is usually done by having each distinct part of the program output important messages to a log file.

Here is a what the log file for a video game might look like:

{% controlledbreakout 600px %}

```plaintext
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
```
{% endcontrolledbreakout %}


Rendering system?
Audio system?
Window creation?
Graphic asset loading?
It doesn't matter what these different systems do, what matters is understanding that each has the capacity to log information.
Or, in other words, each system has access to some kind of logging class.


How do we provide access to a logging class to a large portion of a program?

One way is to create a new instance of the logger class everywhere it is needed.
Whether or not this is a good choice is highly situational, but it is easy to think of potential issues.

In the same way that having two drivers caused a conflict in the direction of our car, how might having two, or more, loggers cause conflicts of state or behavior?


{% containerbreakout %}
<div class="row">
<div class="col-xl-6">

{:.initial-letter}
In our universe, when you're driving a car and want to turn, you turn in the direction you want the car to go.
If you want to go to the supermarket, and the supermarket is to the right, you turn the car right.
If you want to see a movie, and the movie theater is to the left, you turn the car left.

Our cars, more or less, utilize the Singleton pattern.
We limit them to a single instance of a steering system, and that steering system is globally accessible through the steering wheel. [^1]

Now, let's imagine a universe where every car has two steering systems.
The first steering system controls the wheels on the left side of the car, while the second steering system controls the wheels on the right side of the car.
To drive a car in this universe you require two drivers, each controlling a side of the car.

When they want to make a turn, both drivers must turn in the direction they want the car to go.
If they want to go to the supermarket, both drivers must turn right.
If they want to go to the movie theater, both drivers must turn left.

What happens when there is a conflict of interest?
What happens when you want to go to the supermarket, but the other driver wants to go to the movie theater? 

</div>
<div class="col-xl-6">

In our universe, when you're driving a car and want to turn, you turn in the direction you want the car to go.
If you want to go to the supermarket, and the supermarket is to the right, you turn the car right.
If you want to see a movie, and the movie theater is to the left, you turn the car left.

Our cars, more or less, utilize the Singleton pattern.
We limit them to a single instance of a steering system, and that steering system is globally accessible through the steering wheel. [^1]


Now, let's imagine a universe where every car has two steering systems.
The first steering system controls the wheels on the left side of the car, while the second steering system controls the wheels on the right side of the car.
To drive a car in this universe you require two drivers, each controlling a side of the car.

When they want to make a turn, both drivers must turn in the direction they want the car to go.
If they want to go to the supermarket, both drivers must turn right.
If they want to go to the movie theater, both drivers must turn left.

What happens when there is a conflict of interest?
What happens when you want to go to the supermarket, but the other driver wants to go to the movie theater? 

</div>
</div>
{% endcontainerbreakout %}

{% controlledbreakout 700px %}
![painting](/assets/banner.jpg)
{% endcontrolledbreakout %}


{% breakout %}
<p>Hello</p>
{% endbreakout %}

Perhaps the act of attempting to access the same file is enough to cause an error?
How do all the seperate instances of the logger class keep track of which line in the file is the next line to write to?
Is it possible for the loggers to accidently overwrite each other's output?


These are all vaild and important questions to ask.
A logger which overwrites its own output, or doesn't present its output in chronological order, is a useless logger.

In the same way that we used the Singleton pattern to eliminate the conflict of direction in our car example, we could make our logger class a Singleton to eliminate the potential conflicts that might come with multiples instances of a class.

The Singleton pattern would limit our logger to a single instance and provide a global point of access to that instance.
When a portion of the program needs to log information to a file it simple accesses our single instance of the logging class through the global access point and then uses the logger's functionality. [^3]

<div class="key-section">
### The Big Idea

The big idea here is that the Singleton pattern is used when we want a class to be limited to a single instance.

There are many situations in which using the Singleton pattern *might* be wise.
However, in general terms, situations where multiple instances of a class would cause a conflict of state or behavior are prime use cases for the Singleton pattern.
</div>

## Implementing the Singleton 

The Singleton pattern has two responsibilities: It limits a class to a single instance, and it provides a global point of access to that instance.
Accordingly, when implementing the Singleton pattern we have to ask and answer two questions:

{:.alpha}
1. How do we limit a class to a single instance?
2. And how do we provide a global point of access to that instance?


Luckily for us, this turns out to be incredibly simple.
Here is a bare bones implementation of the Singleton pattern in Java:


Not much to look at, eh?
There are three parts to this implementation: The the private constructor, the private static final `INSTANCE` field, and the public static method `getInstance()`.

### The Constructor
Let's start with the constructor.
In Java, the conventional way for creating an instance of a class is with the `new` keyword:

```java
    SomeClass instance = new SomeClass();
```

The above line creates an object of type `SomeClass` and stores it in the variable `instance`.

The Singleton pattern requires us to limit our class to a single instance.
To do this, we have to make sure our Singleton can't be instantiated outside of its class.
We can achieved this with a private constructor:

```java
    private Singleton() { 

    }
```


By declaring our constructor as `private` we make it so that the class can only be instantiated within itself.
Attempting to instantiate our class outside of itself with the `new` keyword is no longer valid.


### The Single Instance
By making our constructor private, we've solved half of the "how do I limit a class to a single instance" problem.
The next step is to create a way for our Singleton class to keep track of its single instance.

~~~java
private static final Singleton INSTANCE = new Singleton();
~~~

The above line declares a private static final field of type `Singleton` called `INSTANCE`.
This is where we create and store the single instance of our class.

We declare `INSTANCE` as `private` so that it isn't directly exposed to the outside world.
We make it `static` so that the field belongs to the class itself.
And we make it `final` so that it can only be assigned once.

The result is a private static final field which hold the one and only instance of our class.

### A Global Point of Access
The final ingredient of in Singleton pattern is a global point of access.
Because the classes constructor and `INSTANCE` field are private, we cannot get an instance of our class without some help.

The help we need is a public static method:

{% controlledbreakout 550px %}
~~~java
public static Singleton getInstance() {
    return INSTANCE;
}
~~~
{% endcontrolledbreakout %}


The convention is to call this method `getInstance()`.
We declare it as `public` so that it can be accessed directly through the class.
And we declare it as `static` so that it belongs to the class itself. [^4]

This method is more a less a getter.
Its only purpose is to return the instance of our class held in the `INSTANCE` field.

### Putting It All Together

If we combine our private constructor, private static final field `INSTANCE`, and our public static method `getInstance()`, we've built a bare bones Singleton:

{% controlledbreakout 550px %}
~~~java
public class Singleton {

    private static final Singleton INSTANCE = new Singleton();

    private Singleton() { 

    }

    public static Singleton getInstance() {
        return INSTANCE;
    }
}
~~~
{% endcontrolledbreakout %}


Together the private constructor and private static final field `INSTANCE` satisfy the requirment of limiting the class to a single instance.
And the `getInstance()` method fulfils the requirement of providing a global point of access to that instance.

Using the Singleton is straightfoward:

{% controlledbreakout 550px %}
~~~java
Singleton instance = Singleton.getInstance();
~~~
{% endcontrolledbreakout %}

All you have to do is store the results of the `getInstance()` method in a variable.
You now have a working Singleton.



### A Basic, But More Concrete Example

In the [What is the Singleton]() section we used the example of a logging class.
Here is what a primitive logging class that uses the Singleton pattern might look like:

{% controlledbreakout 550px %}
~~~java
public Logger {

    private static final Logger INSTANCE = new Logger();

    private Logger() {

    }

    public static Logger getInstance() {
        return INSTANCE;
    }

    public void log(String message) {
        System.out.println(message);
    }

    public void info(String message) {
        this.log("[INFO] " + message);
    }

    public void error(String message) {
        this.log("[ERROR] " + message);
    }
}
~~~
{% endcontrolledbreakout %}


Our `Logger` class has all the parts of the Singleton pattern that we covered in the previous sections.
It has a private static final field `INSTANCE`, a private construtor, and a public static method `getInstance()`.



Additionally, it has three other methods.
To keep things simple this logger doesn't output to a log file, but, instead, outputs to a console.
To use these methods, we must retrieve the instance of our `Logger` class using the `getInstance()` method:

~~~java
    Logger logger = Logger.getInstance();
    logger.info("Here is our singleton logger in action!!!");
~~~

And then we simply call the methods we want.


### Eager vs. Lazy Initialization
In the previous examples, we created the single instance of our classes immediately:

~~~java
private static final Singleton INSTANCE = new Singleton();
~~~

This is called *Eager Initialization*, and it is one of two ways the Singleton pattern is commonly implemented.
The other way is called *Lazy Initialization*.
This is what a Singleton that uses *Lazy Initialization* looks like:

~~~java
public class LazySingleton {

    private static LazySingleton instance = null;

    private LazySingleton() { }

    public static LazySingleton getInstance() {
        if(instance == null) {
            instance = new LazySingleton();
        }

        return instance;
    }
}
~~~

If you compare this implementation with the previous examples, you should noticed two key differences.

~~~java
private static LazySingleton instance = null;
~~~

Firstly, instead of creating the single instance of our class immediately and assigning it to a private static final field, we declare a private static field and set it equal to `null`.

{% controlledbreakout 550px %}
~~~java
public static LazySingleton getInstance() {
    if(instance == null) {
        instance = new LazySingleton();
    }

    return instance;
}
~~~
{% endcontrolledbreakout %}

Secondly, the `getInstance()` method is slightly more complex.
Now, the method will check if the `instance` field is equal to `null`, create the single instance if it is, and return that instance.

The effect of these two changes is that the creation of the single instance of our class is deferred until it is needed.

*Eager Initialization* is used when the Singleton class should be available immediately to the program.
*Lazy Initialization* is used when the Singleton should be created when it is first needed.

Whether you should use *Eager Initialization* or *Lazy Initialization* comes down to the design of the class and your software.

Is the class lightweight, or used every time the program is executed?
Maybe use *Eager Initialization*.
Is the class large, complex, or only used some of the time?
Maybe use *Lazy Initialization*.

### The Constructor
Let's start with the constructor.
In Java, the conventional way for creating an instance of a class is with the `new` keyword:

~~~java
    SomeClass instance = new SomeClass();
~~~

The above line creates an object of type `SomeClass` and stores it in the variable `instance`.

The Singleton pattern requires us to limit our class to a single instance.
To do this, we have to make sure our Singleton can't be instantiated outside of its class.
We can achieved this with a private constructor:

~~~java
    private Singleton() { 

    }
~~~

By declaring our constructor as `private` we make it so that the class can only be instantiated within itself.
Attempting to instantiate our class outside of itself with the `new` keyword is no longer valid.

### The Single Instance
By making our constructor private, we've solved half of the "how do I limit a class to a single instance" problem.
The next step is to create a way for our Singleton class to keep track of its single instance.

~~~java
    private static final Singleton INSTANCE = new Singleton();
~~~

The above line declares a private static final field of type `Singleton` called `INSTANCE`.
This is where we create and store the single instance of our class.

We declare `INSTANCE` as `private` so that it isn't directly exposed to the outside world.
We make it `static` so that the field belongs to the class itself.
And we make it `final` so that it can only be assigned once.

The result is a private static final field which hold the one and only instance of our class.

### A Global Point of Access
The final ingredient of in Singleton pattern is a global point of access.
Because the classes constructor and `INSTANCE` field are private, we cannot get an instance of our class without some help.

The help we need is a public static method:

~~~java
    public static Singleton getInstance() {
        return INSTANCE;
    }
~~~

The convention is to call this method `getInstance()`.
We declare it as `public` so that it can be accessed directly through the class.
And we declare it as `static` so that it belongs to the class itself. [^4]

This method is more a less a getter.
Its only purpose is to return the instance of our class held in the `INSTANCE` field.

### Putting It All Together

If we combine our private constructor, private static final field `INSTANCE`, and our public static method `getInstance()`, we've built a bare bones Singleton:

~~~java
public class Singleton {

    private static final Singleton INSTANCE = new Singleton();

    private Singleton() { 

    }

    public static Singleton getInstance() {
        return INSTANCE;
    }
}
~~~

Together the private constructor and private static final field `INSTANCE` satisfy the requirment of limiting the class to a single instance.
And the `getInstance()` method fulfils the requirement of providing a global point of access to that instance.

Using the Singleton is straightfoward:

~~~java
    Singleton instance = Singleton.getInstance();
~~~

All you have to do is store the results of the `getInstance()` method in a variable.
You now have a working Singleton.


### A Basic, But More Concrete Example

In the [What is the Singleton]() section we used the example of a logging class.
Here is what a primitive logging class that uses the Singleton pattern might look like:

~~~java
public Logger {

    private static final Logger INSTANCE = new Logger();

    private Logger() {

    }

    public static Logger getInstance() {
        return INSTANCE;
    }

    public void log(String message) {
        System.out.println(message);
    }

    public void info(String message) {
        this.log("[INFO] " + message);
    }

    public void error(String message) {
        this.log("[ERROR] " + message);
    }
}
~~~

Our `Logger` class has all the parts of the Singleton pattern that we covered in the previous sections.
It has a private static final field `INSTANCE`, a private construtor, and a public static method `getInstance()`.

Additionally, it has three other methods.
To keep things simple this logger doesn't output to a log file, but, instead, outputs to a console.
To use these methods, we must retrieve the instance of our `Logger` class using the `getInstance()` method:

~~~java
    Logger logger = Logger.getInstance();
    logger.info("Here is our singleton logger in action!!!");
~~~

And then we simply call the methods we want.


### Eager vs. Lazy Initialization
In the previous examples, we created the single instance of our classes immediately:

~~~java
    private static final Singleton INSTANCE = new Singleton();
~~~

This is called *Eager Initialization*, and it is one of two ways the Singleton pattern is commonly implemented.
The other way is called *Lazy Initialization*.
This is what a Singleton that uses *Lazy Initialization* looks like:

~~~java
public class LazySingleton {

    private static LazySingleton instance = null;

    private LazySingleton() { }

    public static LazySingleton getInstance() {
        if(instance == null) {
            instance = new LazySingleton();
        }

        return instance;
    }
}
~~~

If you compare this implementation with the previous examples, you should noticed two key differences.

~~~java
    private static LazySingleton instance = null;
~~~

Firstly, instead of creating the single instance of our class immediately and assigning it to a private static final field, we declare a private static field and set it equal to `null`.

~~~java
    public static LazySingleton getInstance() {
        if(instance == null) {
            instance = new LazySingleton();
        }

        return instance;
    }
~~~

Secondly, the `getInstance()` method is slightly more complex.
Now, the method will check if the `instance` field is equal to `null`, create the single instance if it is, and return that instance.

The effect of these two changes is that the creation of the single instance of our class is deferred until it is needed.

*Eager Initialization* is used when the Singleton class should be available immediately to the program.
*Lazy Initialization* is used when the Singleton should be created when it is first needed.

Whether you should use *Eager Initialization* or *Lazy Initialization* comes down to the design of the class and your software.

Is the class lightweight, or used every time the program is executed?
Maybe use *Eager Initialization*.
Is the class large, complex, or only used some of the time?
Maybe use *Lazy Initialization*.

### The Constructor
Let's start with the constructor.
In Java, the conventional way for creating an instance of a class is with the `new` keyword:

~~~java
    SomeClass instance = new SomeClass();
~~~

The above line creates an object of type `SomeClass` and stores it in the variable `instance`.

The Singleton pattern requires us to limit our class to a single instance.
To do this, we have to make sure our Singleton can't be instantiated outside of its class.
We can achieved this with a private constructor:

~~~java
    private Singleton() { 

    }
~~~

By declaring our constructor as `private` we make it so that the class can only be instantiated within itself.
Attempting to instantiate our class outside of itself with the `new` keyword is no longer valid.

### The Single Instance
By making our constructor private, we've solved half of the "how do I limit a class to a single instance" problem.
The next step is to create a way for our Singleton class to keep track of its single instance.

~~~java
    private static final Singleton INSTANCE = new Singleton();
~~~

The above line declares a private static final field of type `Singleton` called `INSTANCE`.
This is where we create and store the single instance of our class.

We declare `INSTANCE` as `private` so that it isn't directly exposed to the outside world.
We make it `static` so that the field belongs to the class itself.
And we make it `final` so that it can only be assigned once.

The result is a private static final field which hold the one and only instance of our class.

### A Global Point of Access
The final ingredient of in Singleton pattern is a global point of access.
Because the classes constructor and `INSTANCE` field are private, we cannot get an instance of our class without some help.

The help we need is a public static method:

~~~java
    public static Singleton getInstance() {
        return INSTANCE;
    }
~~~

The convention is to call this method `getInstance()`.
We declare it as `public` so that it can be accessed directly through the class.
And we declare it as `static` so that it belongs to the class itself. [^4]

This method is more a less a getter.
Its only purpose is to return the instance of our class held in the `INSTANCE` field.

### Putting It All Together

If we combine our private constructor, private static final field `INSTANCE`, and our public static method `getInstance()`, we've built a bare bones Singleton:

~~~java
public class Singleton {

    private static final Singleton INSTANCE = new Singleton();

    private Singleton() { 

    }

    public static Singleton getInstance() {
        return INSTANCE;
    }
}
~~~

Together the private constructor and private static final field `INSTANCE` satisfy the requirment of limiting the class to a single instance.
And the `getInstance()` method fulfils the requirement of providing a global point of access to that instance.

Using the Singleton is straightfoward:

~~~java
    Singleton instance = Singleton.getInstance();
~~~

All you have to do is store the results of the `getInstance()` method in a variable.
You now have a working Singleton.


### A Basic, But More Concrete Example

In the [What is the Singleton]() section we used the example of a logging class.
Here is what a primitive logging class that uses the Singleton pattern might look like:

~~~java
public Logger {

    private static final Logger INSTANCE = new Logger();

    private Logger() {

    }

    public static Logger getInstance() {
        return INSTANCE;
    }

    public void log(String message) {
        System.out.println(message);
    }

    public void info(String message) {
        this.log("[INFO] " + message);
    }

    public void error(String message) {
        this.log("[ERROR] " + message);
    }
}
~~~

Our `Logger` class has all the parts of the Singleton pattern that we covered in the previous sections.
It has a private static final field `INSTANCE`, a private construtor, and a public static method `getInstance()`.

Additionally, it has three other methods.
To keep things simple this logger doesn't output to a log file, but, instead, outputs to a console.
To use these methods, we must retrieve the instance of our `Logger` class using the `getInstance()` method:

~~~java
    Logger logger = Logger.getInstance();
    logger.info("Here is our singleton logger in action!!!");
~~~

And then we simply call the methods we want.


### Eager vs. Lazy Initialization
In the previous examples, we created the single instance of our classes immediately:

~~~java
    private static final Singleton INSTANCE = new Singleton();
~~~

This is called *Eager Initialization*, and it is one of two ways the Singleton pattern is commonly implemented.
The other way is called *Lazy Initialization*.
This is what a Singleton that uses *Lazy Initialization* looks like:

~~~java
public class LazySingleton {

    private static LazySingleton instance = null;

    private LazySingleton() { }

    public static LazySingleton getInstance() {
        if(instance == null) {
            instance = new LazySingleton();
        }

        return instance;
    }
}
~~~

If you compare this implementation with the previous examples, you should noticed two key differences.

~~~java
    private static LazySingleton instance = null;
~~~

Firstly, instead of creating the single instance of our class immediately and assigning it to a private static final field, we declare a private static field and set it equal to `null`.

~~~java
    public static LazySingleton getInstance() {
        if(instance == null) {
            instance = new LazySingleton();
        }

        return instance;
    }
~~~

Secondly, the `getInstance()` method is slightly more complex.
Now, the method will check if the `instance` field is equal to `null`, create the single instance if it is, and return that instance.

The effect of these two changes is that the creation of the single instance of our class is deferred until it is needed.

*Eager Initialization* is used when the Singleton class should be available immediately to the program.
*Lazy Initialization* is used when the Singleton should be created when it is first needed.

Whether you should use *Eager Initialization* or *Lazy Initialization* comes down to the design of the class and your software.

Is the class lightweight, or used every time the program is executed?
Maybe use *Eager Initialization*.
Is the class large, complex, or only used some of the time?
Maybe use *Lazy Initialization*.

### Thread Safety

This section will be brief.

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

```java

public class Logger {

    private static class Holder {
        static final Singleton INSTANCE = new Singleton();
    }

    private Logger() {

    }

    public static Logger getInstance() {
        return Holder.INSTANCE;
    }

    public void log(String message) {
        System.out.println(message);
    }

}
```

{:.bib-header}
## References
{% bibliography --file singleton %}

[^1]: While the Singleton pattern calls for a global point of access, a car's steering wheel is not really a global point of access. In the computing world, a global point of access is a point that can be accessed everywhere and by anything. In the physical world such points of access do not exist. In this example the word *global* is relative to the setting i.e. the car. When someone inside the car wants to access the single instance of the steering system, they do so through the steering wheel.

[^2]: Obviously, following the Singleton pattern is not the only solution to the problem of conflicting state and behavior. Software design can be as endlessly complex as we want it to be. A scenario in which a car has multiple steering systems could undoubtly be made to work without using a Singleton. However, the Singleton pattern represents *a* solution to the problem. A solution which is fairly simple and straightforward.

[^3]: As stated in the previous footnote, the Singleton patternis not the only solution to the problem of conflicting state and behavior. It is absolutely possible to program a logger that is instantiated where ever it is needed, but manages to avoid the problems given in the example.

[^4]: If we do not declare the `getInstance()` method as `static` then we have no way of accessing the method. If the method was simply declared as `public`, and not `public static`, then we would have no way to access the `getInstance()` method as we would require an instance of the class which would be impossible to retrieve thanks to the private constructor and private `INSTANCE` field.