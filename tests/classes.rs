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

#[test]
fn define_methods() -> utils::Result {
    // utils::setup_logger();
    run!(
        r#"
class Brunch {
  eggs() {}
  bacon() {}
}
"# ->
    )
}

#[test]
fn no_this() -> utils::Result {
    // utils::setup_logger();
    run!(
        r#"
class Scone {
  topping(first, second) {
    print "scone with " + first + " and " + second;
  }
}

var scone = Scone();
scone.topping("berries", "cream");
"# ->
        "scone with berries and cream"
    )
}

#[test]
fn this() -> utils::Result {
    // utils::setup_logger();
    run!(
        r#"
class Nested {
  method() {
    fun function() {
      print this;
    }

    function();
  }
}

Nested().method();
"# ->
        "Nested instance"
    )
}

#[test]
fn say_name() -> utils::Result {
    // utils::setup_logger();
    run!(
        r#"
class Person {
  sayName() {
    print this.name;
  }
}

var jane = Person();
jane.name = "Jane";

var method = jane.sayName;
method();
"# ->
        "Jane"
    )
}

#[test]
fn init() -> utils::Result {
    // utils::setup_logger();
    run!(
        r#"
class CoffeeMaker {
  init(coffee) {
    this.coffee = coffee;
  }

  brew() {
    print "Enjoy your cup of " + this.coffee;

    // No reusing the grounds!
    this.coffee = nil;
  }
}

var maker = CoffeeMaker("coffee and chicory");
maker.brew();
"# ->
        "Enjoy your cup of coffee and chicory"
    )
}

#[test]
fn invoke_field() -> utils::Result {
    // utils::setup_logger();
    run!(
        r#"
class Oops {
  init() {
    fun f() {
      print "not a method";
    }

    this.field = f;
  }
}

var oops = Oops();
oops.field();
"# ->
        "not a method"
    )
}
