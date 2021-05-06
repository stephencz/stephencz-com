---
layout: page
title: About
author: "Stephen Czekalski"
description: "Information about this website and its author Stephen Czekalski."
---

*Last Updated: May 6, 2021*

## Site 

### Purpose

The purpose of my website is to act as a repository for my thoughts, experiences, ideas, and opinions.
I see this website as a kind of life project.
It is something I would like to maintain and grow throughout the course of my life.

That being said, the content of my website mainly takes on a blog format: Dated entries about whatever topics I feel like sharing.

One thing this website isn't is an Internet resume.
A common thing programmers do with their personal website is make it an advertisement for employers.
They say here are the things I know, and here are some examples of my work.
I don't have any problems with people showing off their work and skills.
However, making a website solely to convince employers to hire you seems soulless to me.

My interests have always been widespread. 
While I primarily consider myself a programmer, I have many other hobbies and interests.
To avoid writing about them, because its not what employers want to see, isn't something I am going to do.

### Design

#### Aesthetic and Function

Aesthetically and functionally, my goal is simplicity.

[Research indicates](https://www.nngroup.com/articles/how-users-read-on-the-web/) that the majority of Internet users don't *actually* read.
They scan for the information they want.
I try to lean into this fact.
No fancy colors or complex user interfaces.
Just an easy to scan column of text and figures.

#### Technical

This is a static website.
It is built with [Jekyll](https://jekyllrb.com/).
Static website generators are a great choice for small, personal blogs.
They are easier to maintain, generally faster, and less expensive than frameworks such as Wordpress.

I use Jekyll because its the static site generator I understand best.
I don't like the fact that its built with [Ruby](https://www.ruby-lang.org/en/).
I'm actively planning to move away from Jekyll, most likely towards a custom static site generator.
Accordingly, I try to avoid using custom plugins and keep my markdown as vanilla as possible.

The bulk of the website's code is HTML and SASS.
Currently, I'm not using any Javascript.
For keeping the layout responsive I use [Bootstrap](https://getbootstrap.com/).
I try to limit my use of third-party frameworks keep my content unanchored.

### Hosting

#### Current

This website is hosted on a basic $5/month [DigitalOcean](https://www.digitalocean.com/) droplet.
For domain registration and DNS I'm using Amazon Web Services [Route 53](https://aws.amazon.com/route53/). 
Finally, the website is protected by [Cloudflare](https://www.cloudflare.com/) for free. 
In total, I pay about $7/month for hosting. 
I get 25 GB of storage and a terabyte of bandwidth.

#### Prior to 2020

Before switching to DigitalOcean I used two different hosting providers: HostGator and Amazon Web Services. 

HostGator was my first hosting provider. 
I had a shared hosting planning that cost $11/month. 
It was a managed plan which means that HostGator is responsible for managing the server.
HostGator was restrictive, but it served me well as I took my first steps into web development.

I switched from HostGator to AWS.
Static websites are a prime candidate for an [S3 Bucket](https://aws.amazon.com/s3/) with [Cloudfront](https://aws.amazon.com/cloudfront/).
For low traffic websites this is essentially free web hosting, and its lightning fast.
However, you miss out on the capabilities of a backend server.
Ultimately, I didn't like the unpredictability of the pricing, so I moved to DigitalOcean.

#### Referral Links
* [DigitalOcean](https://m.do.co/c/9b44b3503050)


