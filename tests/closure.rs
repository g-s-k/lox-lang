mod utils;

#[test]
fn inner_outer() -> utils::Result {
    // utils::setup_logger();
    run!(
        r#"
var x = "global";

fun outer() {
  var x = "outer";

  fun inner() {
    print x;
  }

  inner();
}

outer();
"# ->
        "outer"
    )
}

#[test]
fn local() -> utils::Result {
    // utils::setup_logger();
    run!(
        r#"
fun makeClosure() {
  var local = "local";

  fun closure() {
    print local;
  }

  return closure;
}

var closure = makeClosure();
closure();
"# ->
        "local"
    )
}

#[test]
fn fun_argument() -> utils::Result {
    // utils::setup_logger();
    run!(
        r#"
fun makeClosure(value) {
  fun closure() {
    print value;
  }
  return closure;
}

var doughnut = makeClosure("doughnut");
var bagel = makeClosure("bagel");
doughnut();
bagel();
"# ->
        "doughnut",
        "bagel"
    )
}

#[test]
fn deferred_declaration() -> utils::Result {
    // utils::setup_logger();
    run!(r#"
fun outer() {
  var x = "value";
  fun middle() {
    fun inner() {
      print x;
    }

    print "create inner closure";
    return inner;
  }

  print "return from outer";
  return middle;
}

var mid = outer();
var in = mid();
in();
"# ->
        "return from outer",
        "create inner closure",
        "value"
    )
}
