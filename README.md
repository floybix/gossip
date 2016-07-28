# gossip 🐽

So gossip, very emotion, wow.


## Overview

There are at least 3 people simulated in the game. It works better
with 4 or 5.

People have feelings: they may **like**, **fear** or be **angry** with
others.

People hold beliefs about what others feel and what others know.

The aim of the game is to become the most popular, i.e. to have the
most people like you.

Random pairs of people meet and tell each other gossip: feelings or
beliefs. _The listener also learns about the source of a belief (who
told it to the speaker) or of any other belief that led to this one
(see rules below)._

You can not tell someone your feelings for them directly. (Too
embarrassing!)

You can lie. But if the person you lied about hears it, they will be
angry with you. _(Actually, they will be angry with whoever told it to
the person that told them. But that will often be the original liar.)_

You can't lie about your own feelings (such a lie could never be
exposed).

If you don't have anything new to say, you owe gossip for next
time. When you meet the same partner again you must come up with
something new. You may have to lie.

All players react to social situations _(actually, their beliefs about
social situations, which may or may not be correct)_ according to
rules:

#### Rules

* if _x_ likes me then I will like _x_.
* if _x_ is angry with me then I will fear _x_.
* if I like _x_ and _x_ likes _y_ then I will either like _y_ or be angry with _y_.
* if I am angry with _x_ and _y_ is also angry with _x_ then I will like _y_.
* if _x_ fears me and I fear _x_, I will not fear _x_.
* if _x_ is more popular than I am, by 2 or more likes, AND nobody I like
  is angry with _x_, I will like _x_.
* if _x_ spread a lie about me, I will be angry with _x_, and assume that _x_
  does not like me.


## Play

You can [play it here.](https://floybix.github.io/gossip/)

Begin by making up 4 characters. Then select **(Playing as:)** to be one
of the characters. That will start the game. Each character starts
with one random feeling. Keep doing **(Next encounter)** until you've
had enough, then **(End game)** to see who ended up being most popular.


## Code

First, install [Leiningen](http://leiningen.org/).

To get an interactive development environment run:

    lein figwheel

and open your browser at [localhost:3449](http://localhost:3449/).
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

    lein clean

To create a production build run:

    lein do clean, cljsbuild once min

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL.


## Tech

Clojure, Clojurescript, Datalog, Datascript, React, Reagent, SVG, Bootstrap.


## Dedication

For Zari Andrews.


## License

Copyright © 2016 Felix Andrews

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
