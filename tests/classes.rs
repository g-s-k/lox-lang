mod utils;

#[test]
fn object() -> utils::Result {
    // utils::setup_logger();
    run!(
        r#"
class Brioche {}
print Brioche;
"# ->
        "Brioche"
    )
}

#[test]
fn instance() -> utils::Result {
    // utils::setup_logger();
    run!(
        r#"
class Brioche {}
print Brioche();
"# ->
        "Brioche instance"
    )
}

#[test]
#[should_panic]
fn bad_getter() {
    // utils::setup_logger();
    let mut vm = lox_lang::VM::default();
    vm.interpret(
        r#"
var obj = "not an instance";
print obj.field;
"#,
    )
    .unwrap();
}

#[test]
fn setter() -> utils::Result {
    // utils::setup_logger();
    run!(
        r#"
class Toast {}
var toast = Toast();
print toast.jam = "grape";
"# ->
        "grape"
    )
}

#[test]
fn properties() -> utils::Result {
    // utils::setup_logger();
    run!(
        r#"
class Pair {}

var pair = Pair();
pair.first = 1;
pair.second = 2;
print pair.first + pair.second;
"# ->
        "3"
    )
}
