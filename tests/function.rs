mod utils;

#[test]
fn print() -> utils::Result {
    run!(
        r#"
fun areWeHavingItYet() {
  print "Yes we are!";
}

print areWeHavingItYet;
"# ->
        "#<fun areWeHavingItYet/0>"
    )
}

#[test]
fn sum() -> utils::Result {
    run!(
        r#"
fun sum(a, b, c) {
  return a + b + c;
}

print 4 + sum(5, 6, 7);
"# ->
        "22"
    )
}

#[test]
#[should_panic]
fn not_a_function() {
    let mut vm = lox_lang::VM::default();
    vm.interpret(
        r#"
var notAFunction = 123;
notAFunction();
"#,
    )
    .unwrap();
}

#[test]
fn no_return() -> utils::Result {
    run!(
        r#"
fun noReturn() {
  print "Do stuff";
  // No return here.
}

print noReturn(); // ???
"# ->
        "Do stuff",
        "nil"
    )
}

#[test]
#[should_panic]
fn top_level_return() {
    let mut vm = lox_lang::VM::default();
    vm.interpret(
        r#"
return "What?!";
"#,
    )
    .unwrap();
}
