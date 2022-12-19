---
title: Astronomical Calculations for Hard SF in Common Lisp
summary: Worldbuilding with the HYG database and Lisp.
---

In [_The Epiphany of Gliese 581_][eog581], a group of explorers search the
mortal remains of a dead superintelligence. The expedition begins in Beta
Pictoris---today an unremarkable blue star; in the story, a posthuman Dyson
swarm civilization of quadrillions---then passes through Gliese 581, and ends
where it started.

I wanted to build a timeline of the story, and, because this is hard science
fiction, this means doing real math on real astronomical data. I ended up
writing a small framework for doing astronomical calculations. In Common Lisp,
for old time's sake.

# The Problem

Building a timeline requires knowing travel times, which requires knowing the
distances between the stars. Which requires knowing the _positions_ of the
stars[^drift]. So I had to download the [HYG][hyg] database, which has all the
information I need.

_But_. The characters don't travel in a straight-line ballistic
trajectory. They're digital people, so they can travel at the speed of light by
sending their mental states over an interstellar communications network. And,
because lasers decohere with distance, each "jump" is limited to a relatively
short distance. So the fastest route from A to B is not a straight line on a
rocket, but a zig-zagging trajectory on an optical link. To find the fastest
network route I wrote an implementation of [Dijkstra's algorithm][dijkstra] and
ran it over a graph of stars, the edges linking all stars whose distance is less
than the laser cutoff distance.

An added constraint: Gliese 581 is not on the network, because it was inhabited
by a taciturn superintelligence. So I had to write even more code to find the
star _closest_ to Gliese 581, from which the characters complete the last leg of
the journey on a fusion rocket.

[eog581]: https://borretti.me/fiction/eog581
[hyg]: https://github.com/astronexus/HYG-Database
[dijkstra]: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm

# Distances

The most widely used unit of length in astronomy is the parsec, some weird
trigonometry thing about parallax. It's just as geocentric as lightyears but
harder to intuit. We will want to convert to light years to present the results.

We could just use floats, but I have a peculiar malady where I enjoy writing
CLOS class definitions too much. Also, if I don't have distinct types for the
two units, I _will_ confuse them and ruin all subsequent calculations.

```lisp
(defclass light-years ()
  ((value :reader value
          :initarg :value
          :type real
          :documentation "The underlying value."))
  (:documentation "Represents distance in light years."))

(defmethod humanize ((d light-years) stream)
  (let ((d (value d)))
    (format stream "~0,1fly" d)))

(defmethod print-object ((d light-years) stream)
  (print-unreadable-object (d stream :type t)
    (humanize d stream)))

(defclass parsecs ()
  ((value :reader value
          :initarg :value
          :type real
          :documentation "The underlying value."))
  (:documentation "Represents distance in parsecs."))

(defun make-parsecs (value)
  "Create an instance of PARSECS from a numeric value."
  (make-instance 'parsecs :value value))

(defmethod humanize ((d parsecs) stream)
  (let ((d (value d)))
    (format stream "~0,1fpc" d)))

(defmethod print-object ((d parsecs) stream)
  (print-unreadable-object (d stream :type t)
    (humanize d stream)))
```

`humanize` is just so we can compose string representations better:

```lisp
(defgeneric humanize (object stream)
  (:documentation "Return a human-readable representation of an object."))
```

We can easily convert between the two units:

```lisp
(defun light-years-to-parsecs (ly)
  "Convert the given distance in light years to parsecs."
  (make-parsecs (* (value ly) 0.306601)))

(defun parsecs-to-light-years (pc)
  "Convert the given distance in parsecs to light years."
  (make-instance 'light-years :value (* (value pc) 3.26156)))
```

For example:

```lisp
CL-USER> (make-parsecs 5.0)
#<PARSECS 5.0pc>

CL-USER> (parsecs-to-light-years *)
#<LIGHT-YEARS 16.3ly>

CL-USER> (light-years-to-parsecs *)
#<PARSECS 5.0pc>
```

# Star Positions

The positions of stars are given in [equatorial coordinates], a spherical
coordinate system where a position is made up of a right ascension (RA),
declination (DEC), and distance from the Sun (DIST). Two angles and a
radius. For historical reasons, right ascension is reported in
hours-minutes-seconds format, and declination is reported in
degrees-minutes-seconds.

Luckily the HYG database contains the Cartesian (X, Y, Z) coordinates as
well. If it didn't, see below.

# Aside: Equatorial Coordinates

Note of this is strictly necessary, because the HYG database has star positions
both in equatorial and Cartesian coordinates, but I had to solve this problem,
for two reasons. The first is that software is crystallized and verified
understanding: you know you understand something when you can write a computer
program that concretizes that understanding. The second is that when software
solves a problem, it solves it in perpetuity.

## Angles

## Equatorial Coordinates

## Cartesian Coordinates

## Equatorial to Cartesian

# Parsing the HYG Database

The HYG database is a straightforward CSV, so we can parse it easily. We use the
`parse-number` library to parse floats (the alternative is `read-from-string`,
which is heavy and insecure).

First, a class to store the database:

```lisp
(defclass hyg-database ()
  ((stars :reader database-stars
          :initarg :stars
          :type (vector star)
          :documentation "The star vector."))
  (:documentation "The in-memory HYG database."))

(defun star-count (db)
  (length (database-stars db)))
```

And the parsing code is very straightforward:

```lisp
(defun string-or-nil (str)
  (if (string= str "") nil str))

(defun parse-star (cells)
  (destructuring-bind (id hip hd hr gliese bayer proper ra dec dist prma prdec rv mag absmag spect ci x y z &rest etc) cells
    (declare (ignore hr ra dec prma prdec rv mag absmag spect ci etc))
    (make-instance 'star
                   :id (parse-integer id)
                   :proper (string-or-nil proper)
                   :hip (string-or-nil hip)
                   :hd (string-or-nil hd)
                   :gliese (string-or-nil gliese)
                   :bayer (string-or-nil bayer)
                   :distance (make-parsecs (parse-number:parse-real-number dist))
                   :cartesian-position (make-instance 'cartesian-position
                                                      :x (make-parsecs (parse-number:parse-real-number x))
                                                      :y (make-parsecs (parse-number:parse-real-number y))
                                                      :z (make-parsecs (parse-number:parse-real-number z))))))
```

# Nearest Stars

```lisp
(defun find-stars-within-radius (db pos radius)
  "Given an HYG database, a position in Cartesian coordinates, and a radius in
parsecs, return a vector of all the stars that are within the radius from that
position."
  (let ((stars (make-array 0 :adjustable t :element-type 'star :fill-pointer 0)))
    (loop for star across (database-stars db) do
      (let ((star-pos (star-cartesian-position star)))
        (when (< (value (euclidean-distance pos star-pos))
                 (value radius))
          (vector-push-extend star stars))))
    stars))

(defun star-euclidean-distance (a b)
  "The Euclidean distance between two stars in parsecs."
  (euclidean-distance (star-cartesian-position a) (star-cartesian-position b)))
```

# The Network Route

# Star Maps

# Conclusion

All of this had very little consequence to the story: a few throwaway
sentences. But I see it as a matter of respect for the audience. If it's hard
SF, it's hard SF.

# Footnotes

[^drift]:
    What about drift? The HYG database has the velocity vectors of the stars,
    but it doesn't matter. In the short run, stars move too slowly for their
    motion to affect travel times appreciably. In the long run, it's too hard to
    solve: _n_-body with very inaccurate data spanning tens of kiloyears. The
    story doesn't take place far enough into the future for this to matter, so
    all coordinates are [J2000][j2000].

[j2000]: https://en.wikipedia.org/wiki/Epoch_(astronomy)
