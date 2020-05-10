mod utils;

#[test]
fn object() -> utils::Result {
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

#[test]
fn inherit() -> utils::Result {
    run!(
        r#"
class Doughnut {
  cook() {
    print "Dunk in the fryer.";
  }
}

class Cruller < Doughnut {
  finish() {
    print "Glaze with icing";
  }
}
"# ->
    )
}

#[test]
fn super_call() -> utils::Result {
    run!(
        r#"
class A {
  method() {
    print "A method";
  }
}

class B < A {
  method() {
    print "B method";
  }

  test() {
    super.method();
  }
}

class C < B {}

C().test();
"# ->
        "A method"
    )
}

#[test]
fn super_access() -> utils::Result {
    run!(
        r#"
class A {
  method() {
    print "A";
  }
}

class B < A {
  method() {
    var closure = super.method;
    closure(); // Prints "A".
  }
}

B().method();
"# ->
        "A"
    )
}

#[test]
fn super_doughnut() -> utils::Result {
    run!(
        r#"
class Doughnut {
  cook() {
    print "Dunk in the fryer.";
    this.finish();
  }

  finish(ingredient) {
    print "Finish with " + ingredient;
  }
}

class Cruller < Doughnut {
  finish() {
    super.finish("icing");
  }
}

Cruller().cook();
"# ->
        "Dunk in the fryer.",
        "Finish with icing"
    )
}
