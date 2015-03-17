#lang scribble/manual

@title{Gnucash}

@author[(author+email "John Clements" "clements@racket-lang.org")]

@(require (for-label racket)
          (for-label gnucash))

@defmodule[gnucash]{This collection is intended to allow you to use Racket/DrRacket to operate on
gnucash files.  It's flimsy but effective for me. It's intended for Racket
hackers, and is underdocumented, under-test-cased, and under-error-checking-ed.
For heaven's sake don't try using this stuff unless you've already written a
bunch of Racket code.

Olin Shivers talks about 80% solutions; I would call this a 60% solution.  Here's why
you'd use it: it has tests, and they run. Let me know if you improve it.}

@section{How to Use It:}

The very simplest way to see how it works is to take a look at the examples
directory: it contains a simple example gnucash file, and a simple Racket file
with plenty of comments that opens the gnucash file and pulls out a few of the
transactions in the file.  If you still have questions, come back and read this
file.

@section{How to Use It (part II):}

The library consists of two files.  One reads in the data (parse.rkt), and one
has some utility functions for handling it. Both of them are loaded automatically.


@subsection{Parsing}

@defproc[(gnucash-read (gnucash-file path-string?) (gnucash-zo-file path-string?))
         true]{The reader does a funny thing: since XML reading is so appallingly slow, it
makes a .zo file that contains the compiled representation of the input file.
Building this thing is horrifically slow, taking about 3 minutes to process my
3-megabyte gnucash file (actually, it's 19M after unzipping it).  This is all
hidden from you, except that when you call the reader you must supply the name
of a cache file.

If the source file is newer than the zo file, it goes and recompiles the darn
thing.}

You probably won't need to call any other functions in parse.rkt.

@subsection{A Note On Representations}

Darn near everything in this library is represented using plain-old Racket
lists, and more specifically SXML representations.  The up side of this is that
you can "display" nearly everything transparently.  The down side of this is
that you will curse and scream at a bunch of errors that would be caught by any
kind of type system or use of structures, e.g. using an account instead of an
account id, etc.

Note from 2015: Let's use TR! That's definitely a better use of my time than
grading all these exams....

@subsection{Libs.rkt}

Here's how to require it:

@racket[(require gnucash/libs)]

Ooh, this one is yucky. If I had more time to spend on this, I would make this
library a unit that imports a set of transactions from another unit.  This is
because functions like "find-account" need to know about all the accounts in
the world.  Instead of using units (or parameterizing every call to
find-account et. al. by a big global table), I have an init function:

@defproc[(init-libs (list-of-gnucash-things (listof any/c)))
 any/c]{
 Call this function with the result of gnucash-read, to mutate a bunch of lib's
internal variables.  Is this gross?  Yes, it's gross.}

I'm not even going to try to document a significant subset of the functions in
lib.rkt; they're mostly like this one:

> (transaction-splits t)

Returns the splits associated with the transaction

> (transaction-date t)

Returns the date associated with the transaction

Here's one worth mentioning:

> (crossers transactions account-ids)

For most kinds of graphing/reporting/munging/etc., you will want to choose a
set of accounts and look for all transactions that "cross the line", in the
sense that they transfer money from that set of accounts to something else.
More accurately, you want transactions where at least one split is inside the
set of accounts, and where at least one split is outside the set of accounts.

Why is this?  Well, imagine you have a checking account and a credit card.  You
want to take a look at what you're spending your money on.  You'd like to lump
together the checking account and the credit card account and take a look at
the money coming into or out of these two accounts, while ignoring transfers
between these two.  This is what "crossers" is for.

Please do note that the "account-ids" is a list of ids (that is, strings), and
not accounts.  The example file shows this happening.

@section{Example.rkt}

For an example of how to use this, take a look at @path{examples/examples.rkt}.

Well, that's about all I have to say for now.  Let me know if you find it
useful or if it makes you grind your teeth.

John Clements, 2007-08 (updated repeatedly, most recently 2015-03)

