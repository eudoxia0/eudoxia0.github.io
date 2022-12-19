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

The `cartesian-position` class represent as Cartesian triple:

```lisp
(defclass cartesian-position ()
  ((x :reader x
      :initarg :x
      :type parsecs
      :documentation "The X coordinate in parsecs.")
   (y :reader y
      :initarg :y
      :type parsecs
      :documentation "The Y coordinate in parsecs.")
   (z :reader z
      :initarg :z
      :type parsecs
      :documentation "The Z coordinate in parsecs."))
  (:documentation "A position in Cartesian (X, Y, Z) coordinates."))

(defmethod print-object ((p cartesian-position) stream)
  (print-unreadable-object (p stream :type t)
    (with-slots (x y z) p
      (write-string "X=" stream)
      (humanize x stream)
      (write-string " Y=" stream)
      (humanize y stream)
      (write-string " Z=" stream)
      (humanize z stream))))
```

And the `euclidean-distance` function calculates the distance between two points:

```lisp
(defun euclidean-distance (p1 p2)
  "Calculate the Euclidean distance between two Cartesian coordinates.
Returns a value in parsecs."
  (with-slots ((x1 x) (y1 y) (z1 z)) p1
    (with-slots ((x2 x) (y2 y) (z2 z)) p2
      (let ((x1 (value x1))
            (y1 (value y1))
            (z1 (value z1))
            (x2 (value x2))
            (y2 (value y2))
            (z2 (value z2)))
        (make-parsecs (sqrt (+ (expt (- x1 x2) 2)
                               (expt (- y1 y2) 2)
                               (expt (- z1 z2) 2))))))))
```

We can use this like this:

```lisp
CL-USER> (defparameter a
           (make-instance 'cartesian-position
                          :x (make-parsecs 3.4)
                          :y (make-parsecs -6.7)
                          :z (make-parsecs -1.2)))
#<CARTESIAN-POSITION X=3.4pc Y=-6.7pc Z=-1.2pc>

CL-USER> (defparameter b
           (make-instance 'cartesian-position
                          :x (make-parsecs 9.1)
                          :y (make-parsecs 4.3)
                          :z (make-parsecs -7.2)))
#<CARTESIAN-POSITION X=9.1pc Y=4.3pc Z=-7.2pc>

CL-USER> (euclidean-distance a b)
#<PARSECS 13.8pc>
```

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
                                                      :x (parse-parsecs x)
                                                      :y (parse-parsecs y)
                                                      :z (parse-parsecs z)))))

(defun string-or-nil (str)
  (if (string= str "") nil str))

(defun parse-parsecs (str)
  (make-parsecs (parse-number:parse-real-number str)))
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

And this is the driver code. First, we load the database:

```lisp
;;; Load the HYG database.

(defparameter +db+
  (load-hyg-database #p"hygdata_v3.csv"))

(assert (= (star-count +db+) 119614))
```

Then find some stars by name:

```lisp
;;; Find the relevant stars.

(defparameter +sol+ (find-star-by-name +db+ "Sol"))
(defparameter +g581+ (find-star-by-name +db+ "Gl 581"))
(defparameter +g555+ (find-star-by-name +db+ "Gl 555"))
(defparameter +bpic+ (find-star-by-name +db+ "Bet Pic"))
```

Find the stars within 3 parsecs of Gliese 581, sort them, and print them out:

```lisp
(let ((stars (find-stars-within-radius +db+
                                       (star-cartesian-position +g581+)
                                       (make-parsecs 3.0))))
  ;; Sort stars by distance.
  (flet ((dist (star)
           ;; Distance to Gliese 581.
           (star-euclidean-distance +g581+ star)))
    (let ((sorted (sort stars
                        #'(lambda (star-a star-b)
                            (< (value (dist star-a))
                               (value (dist star-b)))))))
      ;; Print the list of stars.
      (format t "Ten stars closest to Gliese 581:~%~%")
      (format t "~8@A ~12@A~%" "Dist" "Star")
      (format t "---------------------~%")
      ; subseq because the first star is Gl581 itself, and because we only want the top 10
      (loop for star across (subseq sorted 1 11) do
        (format t "~6,2fly ~12@A~%"
                (value (parsecs-to-light-years (dist star)))
                (star-name star)))
      (let ((star (elt sorted 1)))
        (format t "~%The star closest to Gliese 581 is ~A at ~0,2fly"
                (star-name star)
                (value (parsecs-to-light-years (dist star))))))))
```

The output here is:

```
Ten stars closest to Gliese 581:

    Dist         Star
---------------------
  4.25ly       Gl 555
  5.15ly      Gl 570B
  5.17ly      Gl 570A
  7.43ly      NN 3877
  7.87ly    Gl 563.2A
  8.17ly      Gl 644B
  8.18ly      Gl 644C
  8.21ly       Gl 628
  8.34ly      Gl 644A
  8.83ly       Gl 643

The star closest to Gliese 581 is Gl 555 at 4.25ly
```

# The Network Route

The shortest path between two points is the Euclidean distance. But rockets are
slow, and light is fast. And the characters, being uploads, have the option of
travelling by simply sending copies of their mind over a communications
network. Optical transceivers have a limited range, but even then, an indirect,
zig-zagging trajectory at the speed of light is faster than a straight-line
trajectory on a spacecraft moving at maybe 10% of the speed of light.

So: what's the shortest path between two stars, when each hop is limited by
technology and economics to some constant distance? To find the answer, I just
implemented Dijkstra's algorithm and ran it over the graph of stars and network
connections.

The `edge` and `graph` classes represent a graph where edges have a cost:

```lisp
(defclass edge ()
  ((start :reader edge-start
          :initarg :start
          :type integer
          :documentation "The ID of the start node.")
   (end :reader edge-end
        :initarg :end
        :type integer
        :documentation "The ID of the end node.")
   (cost :reader edge-cost
         :initarg :cost
         :type number
         :documentation "The cost of traversing this edge."))
  (:documentation "Represents an edge in a graph."))

(defmethod initialize-instance :after ((edge edge) &key)
  (with-slots (start end cost) edge
    ;; Check edges are not degenerate.
    (when (= start end)
      (error "Degenerate edge: start and end IDs are the same: ~A" start))
    ;; Check cost is non-negative.
    (when (< cost 0.0)
      (error "Edge cost is negative: ~A" cost))))

(defclass graph ()
  ((vertices :accessor graph-vertices
             :initarg :vertices
             :type (vector integer)
             :documentation "A vector of vertex IDs.")
   (edges :accessor graph-edges
          :initarg :edges
          :type (vector edge)
          :documentation "A vector of edge objects."))
  (:documentation "Represents a graph."))
```

A convenience function to construct a graph from the vector of edges:

```lisp
(defun make-graph-from-edges (edges)
  "Construct a graph from a vector of edges."
  (let ((vertex-table (make-hash-table :test 'equal)) ; table of seen IDs
        (vertices (make-array 0 :adjustable t :element-type 'integer :fill-pointer 0))) ; vertex accumulator
    (loop for edge across edges do
      (with-slots (start end) edge
        (unless (gethash start vertex-table)
          (vector-push-extend start vertices)
          (setf (gethash start vertex-table) t))
        (unless (gethash end vertex-table)
          (vector-push-extend end vertices)
          (setf (gethash end vertex-table) t))))
    (make-instance 'graph :vertices vertices
                          :edges edges)))
```

Dijkstra:

```lisp
(defun dijkstra (graph source destination)
  "Find a path from the source node to the destination in the given graph.
GRAPH is an instance of GRAPH. SOURCE and DESTINATION are integer vertex IDs.
Returns a vector of integer vertex IDs."
  ;; Table of distances.
  (let ((dist (make-hash-table :test 'equal)))
    (loop for vertex across (graph-vertices graph) do
      (setf (gethash vertex dist) double-float-positive-infinity))
    (setf (gethash source dist) 0.0)
    ;; Table of previous nodes.
    (let ((previous (make-hash-table :test 'equal)))
      (loop for vertex across (graph-vertices graph) do
        (setf (gethash vertex previous) nil))
      ;; Table of neighbor costs.
      (let ((neighbors (make-neighbors graph)))
        ;; Queue.
        (let ((q (make-hash-table :test 'equal)))
          (loop for vertex across (graph-vertices graph) do
            (setf (gethash vertex q) t))
          (loop while (> (hash-table-count q) 0) do
            (let ((u (pop-min q dist)))
              (when (or (= u destination)
                        (= (gethash u dist) double-float-positive-infinity))
                (return))
              ;; Adjust neighbor distances.
              (let ((neighbors (gethash u neighbors)))
                (loop for v being the hash-keys of neighbors do
                  (let ((cost (gethash v neighbors)))
                    (let ((alt (+ cost (gethash u dist))))
                      (when (< alt (gethash v dist))
                        (setf (gethash v dist) alt)
                        (setf (gethash v previous) u))))))))
          ;; Build path
          (build-path destination previous))))))
```

Auxiliary functions:

```lisp
(defun build-path (destination previous)
  (let ((path (make-array 0 :adjustable t :element-type 'integer :fill-pointer 0)))
    (let ((u destination))
      (loop while (gethash u previous) do
        (vector-push-extend u path)
        (setf u (gethash u previous)))
      (vector-push-extend u path)
      (reverse path))))

(defun make-neighbors (graph)
  "Construct the neighbors map. This is a hash table from vertex IDs to a hash
  table of vertex IDs to costs. That is:
Map[ID, Map[ID, Float]]"
  (with-slots (vertices edges) graph
    (let ((neighbors (make-hash-table :test 'equal)))
      ;; Initialize.
      (loop for vertex across vertices do
        (setf (gethash vertex neighbors) (make-hash-table :test 'equal)))
      ;; Fill.
      (loop for edge across edges do
        (with-slots (start end cost) edge
          (setf (gethash end (gethash start neighbors)) cost)
          (setf (gethash start (gethash end neighbors)) cost)))
      ;; Return
      neighbors)))

(defun pop-min (queue dist)
  "The worst priority queue implementation ever written."
  (let ((min-id nil)
        (min-dist nil))
    (loop for vertex being the hash-keys of queue do
      (when (or (null min-id)
                (< (gethash vertex dist) min-dist))
        (setf min-id vertex)
        (setf min-dist (gethash vertex dist))))
    (assert (not (null min-id)))
    (assert (not (null min-dist)))
    (remhash min-id queue)
    min-id))
```

Now we can build the star graph. This function takes a vector of stars, and
creates a graph where the nodes are star IDs, and two nodes are connected if the
distance between two stars is less than the threshold:

```lisp
(defun make-graph (stars dist)
  "Create a graph from a vector of stars. Only create edges between
stars that are less than DIST parsecs apart."
  (let ((n (length stars)) ; Number of stars.
        (edges (make-array 0 :adjustable t :element-type 'edge :fill-pointer 0))) ; edge accumulator
    ;; Do the Cartesian product.
    (loop for i from 0 to (- n 1) do
      (loop for j from (+ i 1) to (- n 1) do
        (let ((a (elt stars i))
              (b (elt stars j)))
          (assert (not (= (star-id a) (star-id b))))
          ;; Find the distance between the two stars.
          (let ((d (euclidean-distance (star-cartesian-position a)
                                       (star-cartesian-position b))))
            ;; If the distance is within the radius, add the edge to the graph.
            (when (<= (value d) (value dist))
              (vector-push-extend (make-instance 'edge
                                                 :start (star-id a)
                                                 :end (star-id b)
                                                 :cost (value d))
                                  edges))))))
    ;; Construct the graph from the edges.
    (make-graph-from-edges edges)))
```

# Star Maps

There aren't any good plotting libraries for Common Lisp, so I used Python's
matplotlib, and used a CSV to generate the data.

Plotting is always tedious, but happily I was able to use ChatGPT to write most
of the plotting code. I asked it to generate a simple example of a scatterplot
with labeled points---that is, stars. Then I modified the presentation a bit,
changing colours and font sizes, but the static plots have the problem that the
perspective makes it hard to know where places really are.

So I showed ChatGPT my plotting code, and asked it to rewrite it to create an
animated where the entire plot is rotated about the vertical axis. It rewrote
the script, preserving bit-for-bit identical output, and added animation
support. I got an inscrutable error, showed it to ChatGPT, and it suggested a
fix.

I want to make three plots:

1. A 3D star map showing the positions of all stars mentioned in the story.
2. A map centered on Gliese 581.
3. A map showing the route from Beta Pictoris to Gliese 555.

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
