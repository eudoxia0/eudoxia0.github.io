/* Algebraic data types. In C */
data Token {
  Integer { int i; };
  String  { char* str; };
}

/* Automatic resource management */
with_open_file(file, "companies.csv", "w+") {
  fwrite("Data!", 1, 5, file);
  /* The file pointer is closed here */
}

/* Lazy evaluation and type inference, too */
var a = delay 10;
/* 'a' is a lambda that takes no arguments, and evaluates to 10 */
