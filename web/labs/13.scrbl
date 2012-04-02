#lang scribble/manual

@(require "../lab.rkt"
          "../unnumbered.rkt"
          "../utils.rkt")

@(define exercise (exercise-counter))

@title[#:tag "lab13"]{4/02: Off to the web we go}

@lab:section{Building an application}

Throughout most of this class, we have programmed small components that would
be part of a larger system. For example, tries could be used in a program that
manipulates text, bloom filters are
@hyperlink["http://blog.alexyakunin.com/2010/03/nice-bloom-filter-application.html"]
          {used in Chrome}
to decide if a website is malicious, and skip lists are used in well-known
programs like @hyperlink["http://redis.io/"]{Redis}.

While these components are important, as a programmer in the real world you
will probably at some point build applications that interact with real users,
connect to the Internet, and use libraries that someone else has developed.

To give you some experience with building an application, we will investigate
what it takes to develop a @hyperlink["http://www.twitter.com"]{Twitter}
client. You will take advantage of many things you've already learned like
programming to interfaces, using iterators, and so on.

@lab:section{Preliminaries}

To make things simple for now, we will share a class Twitter account.
The account name is @racket[CS2510HSpring12] and the password will be
written on the whiteboard in the lab.

Now download the following Java jar files:

...

The @racket[FundiesTwitter.jar] package contains classes that will let you
easily connect to Twitter. The package includes a @racket[FundiesTwitter] class
that gives you the following interface:

@indented{@verbatim|{
  // A FundiesTwitter is a
  //   new FundiesTwitter()
  //
  // and implements:
  //
  // getPublicTimeline : -> List<Status>
  // Gets the top 20 public tweets
  // Effect: connects to Twitter
  //
  // getStatus : -> Status
  // Fetches your status
  // Effect: connects to Twitter
  //
  // setStatus : String -> Void
  // Sets your status
  // Effect: connects to Twitter
}|}

The class described above also uses a @racket[Status] class, which
implements the following interface:

@indented{@verbatim|{
  // A Status implements:
  //
  // getId : -> BigInteger
  // Produces the ID of the tweet
  //
  // getLocation : -> String
  // Produces the location of the tweet
  //
  // getMentions : -> List<String>
  // Produces a list of mentions
  //
  // getText : -> String
  // Produces the contents of the tweet
  //
  // getUser : -> String
  // Produces the user who wrote this tweet
}|}

To get started, implement a @racket[Driver] class that will start up your
application. Recall from lecture that a driver has a static main method
like the following:

@indented{@verbatim|{
  // A Driver implements
  //
  // static main : String[] -> Void
  // Kicks off the program
}|}

@exercise{
  Write a basic application that uses the @racket[FundiesTwitter] class
  to set your class account's status to the string provided at the command-line.

  To provide command-line arguments from Eclipse, go to "Run Configurations"
  and click on the "Arguments" tab. Enter text in the "Program arguments:"
  box.

  Make sure you set the status to the whole string, not just the first part of it.
}

We could like to be able to do more than just set the account's status.
What we will do is make the Twitter application accept several kinds of commands.
For example, if the arguments are @tt{status "Fundies 2 forever"} then your
app should update the status. If it's @tt{timeline}, it should produce the
tweets in the public timeline.

@exercise{
  Have your program operate differently based on the command-line arguments.
  The @tt{status} command should either set or get the status depending on if
  there is another string in the arguments. The @tt{timeline} command should
  print out the timeline.

  To print a value to the command-line, use the @racket[System.out.println]
  static method. Make sure you format tweets from the timeline nicely.
}

This approach works, but sometimes you want to be able to specify multiple
options or commands.

EXPAND EXPAND EXPAND

@exercise{
  Optional: Use the Apache Commons CLI library to parse complicated command-line
  arguments for your program.
}

@lab:section{Now back to the World}

Okay, so you've developed a command-line Twitter client. Most people prefer to
use a graphical interface for their tweets though. Since Java user interface
toolkits are incredibly complicated, we will go back to an old friend:
the World library.

There is a Java version of World that is provided
@hyperlink["http://www.ccs.neu.edu/javalib/FunWorld/"]{here}. It works just
like the @racket[Racket] version of World for the most part.

To get started, you will want to extend the @racket[World] class like this:

@indented{@verbatim|{
  class MyWorld extends World {
    ...
  }
}|}

You will need to define methods like @racket[makeImage] (@racket[to-draw] from
Racket), @racket[onKeyEvent] (@racket[on-key]), @racket[onTick]
(@racket[on-tick]), and so on.

To pass the World's data at each tick, you will want to define your own
constructor so that you can make new @racket[World]s.

@exercise{
  Acquaint yourself with Java's World and write a simple program that
  draws a rocket (or any other vehicle) that travels across the screen.
}

For the next exercise, you will want to familiarize yourself with the
@racket[TextImage] class so that you can draw tweets to the screen.

You may be wondering how you can integrate World (which does not usually use
effects) with your Twitter code which uses a bunch of effects, like
communicating on the Internet. One way to handle this is to call the Twitter
code in @racket[onTick] and use the output from Twitter to create your next
World state.

@exercise{
  Now write a big-bang program that continuously shows updates from
  the class Twitter account.

  You will want to set the speed to something like 30 seconds or 60 seconds like
  @racket[big-bang(500, 300, 30)] so that your application doesn't try to access
  Twitter too often.
}

@exercise{
  Optional: Figure out a good way to set your own status from the keyboard
  and/or mouse.
}

@exercise{
  Optional: If you're feeling more ambitious, create buttons in your World
  that lets you switch from the public timeline to only the accounts you
  are following.
}

@exercise{
  Optional: Also add whatever other features you feel like adding.
}

@lab:section{Going further with Twitter}

The Twitter interface that we have provided in this lab so far has been
simplified quite a bit. It only lets you connect to one account and hides all the
details of connecting to an account. In reality, connecting to a web service
is much more complicated.

The @racket[FundiesTwitter] class actually provides a more complicated
interface to connect to any account. This uses a different constructor:

@indented{@verbatim|{
  // A FundiesTwitter is one of
  //   - new FundiesTwitter()
  //   - new FundiesTwitter(User, Key, Secret)
  //
  // where User, Key, and Secret are String
  //
  // ...
}|}

To make sure that accounts are secure, Twitter uses a complicated system
to ensure that apps only have access to accounts that are actually okay
with giving that permission. As part of that system, Twitter uses an
authentication protocol called OAuth.

With OAuth, an application writer users his/her API key and secret
to sign a request to the service (e.g., Twitter) asking for access to
someone's account. The service then asks the user if this access should be
allowed. If the access is granted, then the application can receive
an @emph{access key} from the service for the account.

If you use the second constructor to @racket[FundiesTwitter], you can
request access to the given user's Twitter account using your application
key and secret.

You can obtain an application key and secret from Twitter's website
@hyperlink["https://dev.twitter.com/apps/new"]{here}.

@exercise{
  Optional: Either obtain your own Twitter application credentials or
  ask the TA for the class account's key and try using the alternative
  constructor to connect to your Twitter account (if you have one).

  You should be redirected to a web page asking you if you want to grant access.
  At the same time, Java will show a dialog asking you to enter a PIN to
  allow access.
}
