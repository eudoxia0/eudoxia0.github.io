---
title: Calling C from MLton
summary: A tutorial on MLton's foreign function interface.
tags: [sml, mlton, tutorial]
---

Without further ado,

~~~sml
val puts = _import "puts": string -> int;

val _ = puts "Hello, world!"
~~~

Compile and run:

~~~bash
$ mlton -default-ann 'allowFFI true' puts.sml
$ ./puts
Hello, world!
~~~

# Using an External Library

Let's port the matrix multiplication example from the [OpenBLAS][blas]
library. First, install the dependencies:

~~~bash
$ sudo apt-get install liblapack-dev liblapack3 libopenblas-base libopenblas-dev
~~~

To verify our result is correct, here's the original C code:

~~~c
#include <cblas.h>
#include <stdio.h>

void main()
{
  int i=0;
  double A[6] = {1.0,2.0,1.0,-3.0,4.0,-1.0};
  double B[6] = {1.0,2.0,1.0,-3.0,4.0,-1.0};
  double C[9] = {.5,.5,.5,.5,.5,.5,.5,.5,.5};
  cblas_dgemm(CblasColMajor, CblasNoTrans, CblasTrans,3,3,2,1,A, 3, B, 3,2,C,3);

  for(i=0; i<9; i++)
    printf("%lf ", C[i]);
  printf("\n");
}
~~~

Compile and run:

~~~bash
$ gcc -o test test.c -lopenblas -lpthread
$ ./test
11.000000 -9.000000 5.000000 -9.000000 21.000000 -1.000000 5.000000 -1.000000 3.000000
~~~

Now, the MLton port:

~~~sml
val dgemm = _import "cblas_dgemm" : int * int * int * int * int * int * real * real array
                                    * int * real array * int * real * real array * int -> unit;

fun main () = let val a : real array = Array.fromList [1.0, 2.0, 1.0, ~3.0, 4.0, ~1.0]
                  and b : real array = Array.fromList [1.0, 2.0, 1.0, ~3.0, 4.0, ~1.0]
                  and c : real array = Array.fromList [0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5]
              in
                  (* The first three numeric constants are enum values from cblas.h *)
                  dgemm (102, 111, 112, 3, 3, 2, 1.0, a, 3, b, 3, 2.0, c, 3);
                  Array.app (fn r => print ((Real.toString r) ^ " ")) c
              end

val _ = main ()
~~~

Compile this with:

~~~bash
$ mlton -default-ann 'allowFFI true' \
        -link-opt '-lopenblas -lpthread' \
        openblas.sml
~~~

And run it:

~~~bash
$ ./openblas
11 ~9 5 ~9 21 ~1 5 ~1 3
~~~

# Extracting Strings

The MLton [documentation][doc] says:

>When calling an imported C function from SML that returns an array, ref, or
>vector result [...] then the object must be an ML object allocated on the ML
>heap.

So if you want to bind a function that returns `char*`, for example, you can't
automatically get MLton to give you an SML string result. To get a string from a
pointer you essentially have to call `strlen` on the pointer and build up the
string by hand. That will probably be the topic of a future blog post.

[blas]: https://github.com/xianyi/OpenBLAS
[doc]: http://mlton.org/ForeignFunctionInterfaceTypes
