/* Algebraic data types */
data Token {
  Integer { int i; };
  String  { char* str; };
}

/* Automatic resource management */
with_open_file(file, "companies.csv", "w+") {
  fwrite("IBM,1911,Public", 1, 15, file);
  /* The file pointer is closed here */
}

/* Lazy evaluation and type inference, too */
var future = delay 10;
/* 'future' is a lambda that takes no arguments,
   and evaluates to 10 */
var value = force future;
/* 'value' is the integer 10 */
