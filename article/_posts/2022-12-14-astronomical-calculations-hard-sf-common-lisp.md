---
title: Astronomical Calculations for Hard SF in Common Lisp
summary: Worldbuilding with the HYG database and Lisp.
math: yes
---

In [_The Epiphany of Gliese 581_][eog581], a group of explorers search the
mortal remains of a dead superintelligence. The expedition begins in Beta
Pictoris---today an unremarkable blue star; in the story, a posthuman [Dyson
swarm][swarm] civilization of quadrillions---then passes through [Gliese
581][g581], and ends where it started.

I wanted to build a timeline of the story, and, because this is hard science
fiction, this means doing real math on real astronomical data. I ended up
writing a small framework for doing astronomical calculations. In Common Lisp,
for old time's sake.

The code is in [this repo][repo]. The rest of this post is a walkthrough of the
astronomy framework, followed by the story-specific code.

[swarm]: https://en.wikipedia.org/wiki/Dyson_sphere#Dyson_swarm
[g581]: https://en.wikipedia.org/wiki/Gliese_581
[repo]: https://github.com/eudoxia0/astro-eog581

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

We can easily convert between the two units by multiplying by a constant:)

```
PC = LY * 0.306601

LY = PC * 3.26156
```

In Common Lisp these can be implemented as:

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

Star positions are typically given in [equatorial coordinates][equatorial], a
spherical coordinate system where positions in space are represented by two
angles (called right ascension (RA) and declination (DEC)), and a radius (the
star's distance from the Sun). For historical reasons, right ascension is
reported in hours-minutes-seconds format, and declination is reported in
degrees-minutes-seconds.

[equatorial]: https://en.wikipedia.org/wiki/Equatorial_coordinate_system

Luckily, the HYG database contains the Cartesian (X, Y, Z) coordinates[^coords]
as well. This makes it much easier to calculate distances and, later, to make
star maps (see the appendix for dealing with equatorial coordinates).

The `cartesian-position` class represents a Cartesian triple:

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

And another to represent the star data we care about:

```lisp
(defclass star ()
  ((id :reader star-id
       :initarg :id
       :type integer
       :documentation "The star's ID in the HYG database.")
   (proper :reader star-proper
           :initarg :proper
           :type (or null string)
           :documentation "The star's proper name, if known.")
   (hip :reader star-hip
        :initarg :hip
        :type (or null string)
        :documentation "The star's name in the Hipparcos catalog, if known.")
   (hd :reader star-hd
       :initarg :hd
       :type (or null string)
       :documentation "The star's name in the Henry Draper catalog, if known.")
   (gliese :reader star-gliese
        :initarg :gliese
        :type (or null string)
           :documentation "The star's name in the the third edition of the Gliese Catalog of Nearby Stars, if known.")
   (bayer :reader star-bayer
          :initarg :bayer
          :type (or null string)
          :documentation "The star's Bayer / Flamsteed designation, if known.")
   (distance :reader star-distance
             :initarg :distance
             :type parsecs
             :documentation "The star's distance from the Sun in parsecs.")
   (cartesian-position :reader star-cartesian-position
                       :initarg :cartesian-position
                       :documentation "The star's Cartesian (X, Y, Z) position."))
  (:documentation "Represents a star from the HYG database."))

(defun star-name (star)
  "A star's name. The following are tried in order: proper name, Bayer
designation, Gliese name, HIP name, HD name. If the star doesn't have any names,
returns '?'."
  (with-slots (proper bayer gliese hip) star
    (or proper
        bayer
        gliese
        (if hip
            (concatenate 'string "HIP " hip)
            "?"))))

(defmethod print-object ((star star) stream)
  (print-unreadable-object (star stream :type t)
    (format stream "~A" (star-name star))))
```

And the parsing code is very straightforward:

```lisp
(defun load-hyg-database (pathname)
  "Load the HYG database from a CSV file."
  (with-open-file (stream pathname :direction :input)
    ;; Discard the header.
    (read-line stream nil)
    (let ((stars (make-array 0 :adjustable t :element-type 'star :fill-pointer 0)))
      (loop for line = (read-line stream nil)
            while line
            do
               (let ((columns (uiop:split-string line :separator ",")))
                 (let ((star (parse-star columns)))
                   (vector-push-extend star stars))))
      (make-instance 'hyg-database :stars stars))))

(defun parse-star (cells)
  "Parse a star from a row (a list of CSV cells)."
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

And, finally, some code to find stars by name:

```lisp
(defun find-star-by-name (db name)
  (loop for star across (database-stars db) do
    (when (string= name (star-name star))
      (return-from find-star-by-name star)))
  nil)
```

Now we can query the database:

```lisp
CL-USER> (defparameter db (load-hyg-database #p"hygdata_v3.csv"))
#<HYG-DATABASE {1007007A63}>

CL-USER> (defparameter star (find-star-by-name db "Gl 581"))
#<STAR Gl 581>

CL-USER> (star-name star)
"Gl 581"

CL-USER> (star-distance star)
#<PARSECS 6.2pc>

CL-USER> (star-cartesian-position star)
#<CARTESIAN-POSITION X=-4.0pc Y=-4.7pc Z=-.8pc>
```

# Nearest Stars

Since Gliese 581 is not on the network, we need to find the star closest to
it. First, this function returns all stars within a given radius of a position:

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
```

Now, the driver code. First, we load the database:

```lisp
(defparameter +db+
  (load-hyg-database #p"hygdata_v3.csv"))

(assert (= (star-count +db+) 119614))
```

Then we find all the stars within 3 parsecs of Gliese 581, sort them, and print
them out:

```lisp
(defparameter +g581+ (find-star-by-name +db+ "Gl 581"))

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

(defun star-euclidean-distance (a b)
  "The Euclidean distance between two stars in parsecs."
  (euclidean-distance (star-cartesian-position a) (star-cartesian-position b)))
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

Now we have to find a route from Beta Pictoris to Gliese 555.

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

This convenience function builds a graph object from the vector of edges:

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

Given a graph whose edges have a cost, Dijkstra's algorithm finds the cheapest
path between given start and end nodes. When cost represents distance, this is
the shortest path.

At the highest level: Dijkstra is breadth-first beam search on a tree rooted at
the start vertex.

More detailed: Dijkstra begin with the start vertex, and performs breadth-first
search by building up a tree through the neighbouring nodes. Each leaf node
knows its cost, that is, the sum of the edge costs in the path from the root to the
leaf. In normal breadth-first search, the search order is just the iteration
order of the node's children. Dijkstra picks the next node that minimizes
cost. That's the beam part of beam search.

Even more detailed, here's the full pseudocode:

**Dijkstra's Algorithm**

1. Inputs:
   1. $G : \text{Graph}$
   1. $V_i: \text{Vertex}$: the start vertex.
   1. $V_f: \text{Vertex}$: the end vertex.
1. Let:
   1. $D: \text{Map}[\text{Vertex}, \mathbb{R}]$ is the table of distances from
      $V_i$ to every other vertex. Initially, we set $D[V_i] = 0$ and $D[v] =
      +\infty, \forall v \in G$ for all other vertices.
   1. $L : \text{Map}[\text{Vertex}, \text{Option}[\text{Vertex}]]$ is the previous links
      table, which keeps track of the path we build while the algorithm runs. It
      maps a vertex to the previous vertex in the path. Initially, $L[v] =
      \text{NIL}, \forall v \in G$.
   1. $Q : \text{Queue}[\text{Vertex}]$ is a [priority queue][queue] of vertices
      ordered by $D[v]$. This is initialized to contain every vertex in
      $G$. This supports one operation, $\text{pop}$, which takes the vertex $v$
      with minimum value of $D[v]$, removes it from $Q$, and returns it.
1. While $Q$ is non-empty:
   1. Let $u = \text{pop}(Q)$.
   1. If $u = V_f \lor D[u] = +\infty$:
      1. Break out of the loop.
   1. Else:
      1. For each pair $(v, c)$ in the neighbours of $u$:
         1. Let $d = c + D[u]$.
         1. If $d < D[v]$:
             1. $D[v] = d$.
             1. $L[v] = u$.
1. Let $P: \text{List}[\text{Vertex}] = ()$.
1. Let $l = V_f$.
1. While $L[l] \neq \text{NIL}$:
   1. Append $l$ to $P$.
   2. $l = P[l]$.
1. Reverse $P$ and return it.

[queue]: https://en.wikipedia.org/wiki/Priority_queue

In Common Lisp we can realize this as follows:

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

Using the following auxiliary functions:

```lisp
(defun build-path (destination previous)
  "Given a vertex ID, and a hash table from vertex IDs to vertex IDs,
follow the path starting from DESTINATION through the hash table, and
return a vector of vertex IDs from the last visited node to the DESTINATION."
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

The HYG database has over 100k stars, but all the stars in the story are very
close to the Sun. So we can pare down the graph considerably by dropping every
star over 70 light years (22 parsecs) from the Sun:

```lisp
(defparameter +stars+
  (remove-if #'(lambda (star)
                 (> (value (star-distance star)) 22.0))
             (copy-seq (database-stars +db+))))
```

Interstellar laser links reach ~16 light years (5 parsecs).

```lisp
(defparameter +laser-limit+ (make-parsecs 5.0))
```

(This number is completely arbitrary, and arguably too high.)

Now we build the star graph:

```lisp
(defparameter +graph+
  (make-graph +stars+ +laser-limit+))

(format t "Star graph has ~A vertices and ~A edges.~%~%"
        (length (graph-vertices +graph+))
        (length (graph-edges +graph+)))
```

And run Dijkstra's algorithm to get the shortest network route and print it out:

```lisp
(defparameter +path+
  (loop for id across (dijkstra +graph+ (star-id +bpic+) (star-id +g555+))
        collecting (find id (database-stars +db+) :key #'star-id)))

(format t "Network route has ~A jumps.~%~%" (1- (length +path+)))

;;; Print the network route.

(format t "~12@A ~12@A ~10@A~%" "Start" "End" "Dist")
(format t "------------------------------------~%")
(loop for (a b) on +path+ by #'cdr while b do
  (format t "~12@A ~12@A ~8,2fly~%"
          (star-name a)
          (star-name b)
          (value (parsecs-to-light-years (star-euclidean-distance a b)))))
```

The output is:

```
Star graph has 2333 vertices and 32266 edges.

Network route has 7 jumps.

       Start          End       Dist
------------------------------------
     Bet Pic       Gl 238    14.08ly
      Gl 238    HIP 27887    12.11ly
   HIP 27887    HIP 31293    16.27ly
   HIP 31293    HIP 31292     1.10ly
   HIP 31292    HIP 58910    15.46ly
   HIP 58910    Gl 563.2A    12.99ly
   Gl 563.2A       Gl 555     6.24ly
```

Now let's add up the distances, and compare it to the Euclidean distance:

```lisp
(format t "Distance from Beta Pictoris to Gliese 555: ~,2fly~%~%"
        (value (parsecs-to-light-years (star-euclidean-distance +bpic+ +g555+))))

(let ((length 0.0))
  (loop for (a b) on +path+ by #'cdr while b do
    (incf length (value (star-euclidean-distance a b))))
  (format t "Total network route length: ~,2fly~%"
          (value (parsecs-to-light-years (make-parsecs length)))))
```

This outputs:

```
Distance from Beta Pictoris to Gliese 555: 70.80ly

Total network route length: 78.25ly
```

A journey on the network is eight years longer than a straight-line journey, but
fusion rockets are limited to ~10% of the speed of light, so the straight-line
ballistic trajectory would be 700 years.

# Star Maps

I wanted to make a map of all the stars mentioned in the story, as well as a map
showing the network route. Part to verify that the calculations were actually
correct, part to have a better understanding of the story's geography: a picture
is easier to understand that a table of numbers.

The simplest way to do this---without writing my own graphics code---is to use a
3D scatter plot. There aren't any good plotting libraries for Common Lisp, so I
used Python's [matplotlib][matplotlib], and used a CSV to transfer the data.

[matplotlib]: https://matplotlib.org/

Plotting is always tedious, but happily I was able to use [ChatGPT][chatgpt] to
write most of the plotting code. I asked it to generate a simple example of a
scatterplot with labeled points---that is, stars. Then I modified the
presentation a bit, changing colours and font sizes, but the static plots have
the problem that the perspective makes it hard to know where places really are.

[chatgpt]: https://openai.com/blog/chatgpt/

So I showed ChatGPT my plotting code, and asked it to rewrite it to create an
animated where the entire plot is rotated about the vertical axis. It rewrote
the script, preserving bit-for-bit identical output, and added animation
support. I got an inscrutable error, showed it to ChatGPT, and it suggested a
fix. I'm really happy with this approach.

I'll start with the output first. Here's the animated map of all place names
mentioned in the story[^libra]:

<video width="100%" autoplay=true loop=true>
  <source src="/assets/content/astronomical-calculations-hard-sf-common-lisp/all-stars.mp4" type="video/mp4" />
</video>

Ararat is the in-universe name of [Gliese 570A][g570], Tigranes is [HD
35650][tig].

[g570]: https://en.wikipedia.org/wiki/Gliese_570
[tig]: http://simbad.u-strasbg.fr/simbad/sim-id?Ident=%40790560&Name=HD%20%2035650&submit=submit

Zoomed in around Gliese 581 for clarity:

<video width="100%" autoplay=true loop=true>
  <source src="/assets/content/astronomical-calculations-hard-sf-common-lisp/g581-environs.mp4" type="video/mp4" />
</video>

And this is the network route from Ctesiphon to Wepwawet:

<video width="100%" autoplay=true loop=true>
  <source src="/assets/content/astronomical-calculations-hard-sf-common-lisp/route.mp4" type="video/mp4" />
</video>

To transfer data from Common Lisp to Python, I dumped the stars to different
CSVs, one for each plot, with `X,Y,Z,Label` as the columns. I won't post the
code since it's very ordinary.

The main body of the Python plotting code (sans CSV parsing, etc.) is:

```python
def plot_stars(input_path, output_path, route=False):
    """
    Plot the stars from the CSV in `input_path`, writes an MP4
    animated scatterplot to `output_path`.

    If `route=True`, draws lines between adjacent points.
    """
    # Data.
    x: list[float]    = []
    y: list[float]    = []
    z: list[float]    = []
    labels: list[str] = []

    # Parse the CSV.
    # snipped

    # Create a figure and a 3D Axes.
    dpi = 600
    fig = plt.figure(figsize=(2,2), dpi=dpi)
    ax = fig.add_subplot(111, projection='3d')

    # Create the scatterplot.
    scatter = ax.scatter(x, y, z, c='b', marker='.', s=1, alpha=0.5)

    # Add labels to each star.
    text = [
        ax.text(x[i], y[i], z[i] + 0.1, label, fontsize=2) for i, label in enumerate(labels)
    ]

    # Draw the impulses.
    impulses = [
        ax.plot([x[i], x[i]], [y[i], y[i]], [0, z[i]], ':', c='k', linewidth=0.2) for i in range(len(x))
    ]

    # Are we plotting a route? If so, draw the lines between the stars.
    if route:
        lines = [
            ax.plot(
                [x[i], x[i+1]],
                [y[i], y[i+1]],
                [z[i], z[i+1]],
                color='r',
                linewidth=0.1
            )
            for i in range(len(labels)-1)
        ]
    else:
        lines = []

    # Plot the origin plane.
    GAP = 1
    X, Y = np.meshgrid(
        np.linspace(min(x) - GAP, max(x) + GAP, 10),
        np.linspace(min(y) - GAP, max(y) + GAP, 10)
    )
    Z = np.zeros_like(X)
    origin_plane = ax.plot_wireframe(X, Y, Z, rstride=1, cstride=1, linewidths=0.1)

    # Hide the axes and grid planes.
    plt.axis("off")

    # Define the animation function.
    def animate(i):
        ax.view_init(elev=30, azim=i)

    # Create the animation object.
    anim = animation.FuncAnimation(fig, animate, frames=360, interval=20, blit=False)

    # Save the animation as an MP4 file.
    anim.save(output_path, fps=30, extra_args=['-vcodec', 'libx264'], dpi=dpi)
```

# Conclusion

All of this had very little consequence to the story: a few throwaway
sentences. But I see it as a matter of respect for the audience. If it's hard
SF, it's hard SF.

# Appendix: Equatorial Coordinates

This section describes to how accurately represent positions in equatorial
coordinates, and how to convert them to Cartesian coordinates for ease of use.

None of this is strictly necessary, because the HYG database has star positions
both in equatorial and Cartesian coordinates, but I had to solve this problem,
for two reasons. The first is that software is crystallized and verified
understanding: you know you understand something when you can write a computer
program that concretizes that understanding. The second is that when software
solves a problem, it solves it in perpetuity.

## Representation

A position in equatorial coordinates is a right ascension (RA, an angle, in
hours-minutes-seconds), a declination (DEC, an angle, in
degrees-minutes-seconds), and a distance from the Sun in parsecs.

The `equatorial-position` class represents this:

```lisp
(defclass equatorial-position ()
  ((right-ascension :reader right-ascension
                    :initarg :right-ascension
                    :type hms-degrees
                    :documentation "The right ascension in HMS.")
   (declination :reader declination
                :initarg :declination
                :type dms-degrees
                :documentation "The declination in DMS.")
   (distance :reader distance
             :initarg :distance
             :type parsecs
             :documentation "The distance in parsecs."))
  (:documentation "A position in equatorial (RA, DEC, DIST) coordinates."))

(defmethod print-object ((p equatorial-position) stream)
  (print-unreadable-object (p stream :type t)
    (with-slots (right-ascension declination distance) p
      (write-string "RA=" stream)
      (humanize right-ascension stream)
      (write-string " DEC=" stream)
      (humanize declination stream)
      (write-string " D=" stream)
      (humanize distance stream))))
```

We could convert the right ascension and declination to angles eagerly, but I'd
rather represent things explicitly, and add conversion functions as needed.

We have three representations of angles: decimal degrees, hours-minutes-seconds,
and degrees-minutes-seconds. Each corresponds to a CLOS class:

```lisp
(defclass decimal-degrees ()
  ((value :reader value
          :initarg :value
          :type real))
  (:documentation "Represents an angle in decimal degrees."))

(defclass hms-degrees ()
  ((hours :reader hours
          :initarg :hours
          :type real)
   (minutes :reader minutes
            :initarg :minutes
            :type real)
   (seconds :reader seconds
            :initarg :seconds
            :type real))
  (:documentation "Represents an angle in HMS (hours-minutes-seconds) format."))

(defclass dms-degrees ()
  ((degrees :reader degrees
            :initarg :degrees
            :type real)
   (minutes :reader minutes
            :initarg :minutes
            :type real)
   (seconds :reader seconds
            :initarg :seconds
            :type real))
  (:documentation "Represents an angle in DMS (degrees-minutes-seconds) format."))
```

We use `initialize-instance` methods to verify that all the values are in range:

```lisp
(defmethod initialize-instance :after ((d decimal-degrees) &key)
  (let ((d (value d)))
    (unless (and (> d -180.0) (<= d 180.0))
      (error "Value out of range for decimal degrees: ~A" d))))

(defmethod initialize-instance :after ((angle hms-degrees) &key)
  (with-slots (hours minutes seconds) angle
    (unless (and (>= hours 0.0) (< hours 24.0))
      (error "Invalid value for hours in hms-degrees: ~a" hours))
    (unless (and (>= minutes 0.0) (< minutes 60.0))
      (error "Invalid value for minutes in hms-degrees: ~a" minutes))
    (unless (and (>= seconds 0.0) (< seconds 60.0))
      (error "Invalid value for seconds in hms-degrees: ~a" seconds))))

(defmethod initialize-instance :after ((angle dms-degrees) &key)
  (with-slots (degrees minutes seconds) angle
    (unless (and (> degrees -90.0) (<= degrees 90.0))
      (error "Invalid value for degrees in hms-degrees: ~a" degrees))
    (unless (and (>= minutes 0.0) (< minutes 60.0))
      (error "Invalid value for minutes in hms-degrees: ~a" minutes))
    (unless (and (>= seconds 0.0) (< seconds 60.0))
      (error "Invalid value for seconds in hms-degrees: ~a" seconds))))
```

## Angle Conversion

To convert from HMS and DMS to decimal, we have these functions:

```lisp
(defun hms-to-decimal (hms)
  "Convert an HMS (hours-minutes-seconds) angle to decimal degrees."
  (with-slots (hours minutes seconds) hms
    (let ((d (+ (* 15.0 hours)
                (/ (* 15.0 minutes) 60.0)
                (/ (* 15.0 seconds) 3600.0))))
      (make-instance 'decimal-degrees :value d))))

(defun dms-to-decimal (dms)
  "Convert a DMS (degrees-minutes-seconds) angle to decimal degrees."
  (with-slots (degrees minutes seconds) dms
    (let ((d (* (sign degrees)
                (+ (abs degrees)
                   (/ minutes 60.0)
                   (/ seconds 3600.0)))))
      (make-instance 'decimal-degrees :value d))))

(defun sign (x)
  (cond ((< x 0.0)
         -1)
        ((= x 0.0)
         0)
        (t
         1.0)))
```

And we can use these like so:

```lisp
CL-USER> (make-instance 'hms-degrees :hours 7.2 :minutes 2.24 :seconds 1.42)
#<HMS-DEGREES 7.2h2.2m1.4s>

CL-USER> (hms-to-decimal *)
#<DECIMAL-DEGREES 108.6°>

CL-USER> (make-instance 'dms-degrees :degrees 51.2 :minutes 2.24 :seconds 1.42)
#<DMS-DEGREES 51.2°2.2m1.4s>

CL-USER> (dms-to-decimal *)
#<DECIMAL-DEGREES 51.2°>
```

Implementing `decimal-to-hms` and `decimal-to-dms` is left as an exercise to the
reader.

## Equatorial to Cartesian

I got the formula from the [Atomic Rockets][atom] website:

[atom]: http://www.projectrho.com/public_html/starmaps/trigonometry.php

```lisp
(defun equatorial-to-cartesian (pos)
  "Convert a position from equatorial to cartesian coordinates."
  (with-slots (right-ascension declination distance) pos
    (let ((φ (value (hms-to-decimal right-ascension)))
          (θ (value (dms-to-decimal declination)))
          (ρ (value distance)))
      (let ((rvect (* ρ (cosr θ))))
        (let ((x (* rvect (cosr φ)))
              (y (* rvect (sinr φ)))
              (z (* ρ     (sinr θ))))
          (make-instance 'cartesian-position
                         :x (make-parsecs x)
                         :y (make-parsecs y)
                         :z (make-parsecs z)))))))

(defun rad (x)
  (* x 0.0174532925))

(defun sinr (x) (sin (rad x)))
(defun cosr (x) (cos (rad x)))
```

We can use this like so:

```lisp
CL-USER> (defparameter tau-ceti
  (make-instance 'equatorial-position
                 :right-ascension (make-instance 'hms-degrees :hours 1 :minutes 41 :seconds 45)
                 :declination (make-instance 'dms-degrees :degrees -16.0 :minutes 12.0 :seconds 0.0)
                 :distance (make-parsecs 3.61)))
#<EQUATORIAL-POSITION RA=1.0h41.0m45.0s DEC=-16.0°12.0m.0s D=3.6pc>

CL-USER> (equatorial-to-cartesian tau-ceti)
#<CARTESIAN-POSITION X=3.1pc Y=1.5pc Z=-1.0pc>
```

# Footnotes

[^drift]:
    What about drift? The HYG database has the velocity vectors of the stars,
    but it doesn't matter. In the short run, stars move too slowly for their
    motion to affect travel times appreciably. In the long run, it's too hard to
    solve: _n_-body with very inaccurate data spanning tens of kiloyears. The
    story doesn't take place far enough into the future for this to matter, so
    all coordinates are [J2000][j2000].

[j2000]: https://en.wikipedia.org/wiki/Epoch_(astronomy)

[^coords]:
    The Cartesian coordinate system used by the HYG database has:

    1. $+X$ towards the vernal point in J2000 (RA 0h, DEC 0°, in the constellation Pisces).

    2. $+Y$ towards RA 6h, DEC 0° (in Monoceros).

    3. $+Z$ towards the north celestial pole.

    The plane $Z=0$ is the plane of the ecliptic.

[^libra]:
    This is a simple check that the math is correct. Note that Gliese 581 is
    close to the plane of the ecliptic, while Ctesiphon (Beta Pictoris) is far
    to galactic south. Gliese 581 has a declination of -7°, while Beta Pictoris
    is -51°. If you look at a [map] of the constellations, Libra (where Gliese
    581 is) is just off the ecliptic, while Pictor is far to the south. They are
    also in roughly opposite directions in the sky.

[map]: https://upload.wikimedia.org/wikipedia/commons/8/89/Constellations%2C_equirectangular_plot%2C_Menzel_families.svg
