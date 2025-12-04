use crate::{
    bytecode_vm::{compiler::CodeObject, Compiler, Runtime, VirtualMachine, VmValue},
    core::{log, Container, Interpreter, LogLevel},
    domain::{MemphisValue, ModuleName, Source},
    errors::{MemphisError, MemphisResult},
    parser::Parser,
    runtime::MemphisState,
};

pub struct VmInterpreter {
    compiler: Compiler,
    vm: VirtualMachine,
}

impl VmInterpreter {
    pub fn new(
        module_name: ModuleName,
        state: Container<MemphisState>,
        runtime: Container<Runtime>,
        source: Source,
    ) -> Self {
        Self {
            compiler: Compiler::new(
                module_name,
                source.path().to_str().expect("Failed to convert path."),
            ),
            vm: VirtualMachine::new(state, runtime),
        }
    }

    pub fn compile(&mut self, parser: &mut Parser) -> MemphisResult<CodeObject> {
        let mut ast = parser.parse().map_err(MemphisError::Parser)?;
        // This simulates CPython `eval` mode, which we assume to be for the tests and REPL.
        // TODO make this explicit with `CompileMode` or similar.
        ast.rewrite_last_expr_to_return();
        self.compiler.compile(&ast).map_err(MemphisError::Compiler)
    }

    pub fn execute(&mut self, parser: &mut Parser) -> MemphisResult<VmValue> {
        let code = self.compile(parser)?;
        log(LogLevel::Trace, || format!("{code}"));
        self.vm.execute(code).map_err(MemphisError::Execution)
    }

    pub fn read_global(&self, name: &str) -> Option<VmValue> {
        self.vm.read_global(name).ok()
    }
}

impl Interpreter for VmInterpreter {
    fn run(&mut self, parser: &mut Parser) -> MemphisResult<MemphisValue> {
        self.execute(parser).map(Into::into)
    }

    fn read(&mut self, name: &str) -> Option<MemphisValue> {
        self.read_global(name).map(Into::into)
    }
}

#[cfg(test)]
mod tests_vm_interpreter {
    use super::*;

    use crate::{
        bytecode_vm::{
            runtime::{
                types::{Dict, List, Range, Tuple},
                Reference,
            },
            test_utils::*,
        },
        domain::test_utils::*,
    };

    impl VmInterpreter {
        pub fn vm(&self) -> &VirtualMachine {
            &self.vm
        }
    }

    #[test]
    fn expression() {
        let text = "4 * (2 + 3)";
        assert_eval_eq!(text, int!(20));

        let text = "4 > x";
        let e = eval_expect_error(text);
        assert_name_error!(e, "x");

        let text = "y()";
        let e = eval_expect_error(text);
        assert_name_error!(e, "y");
    }

    #[test]
    fn binary_addition() {
        let text = "4 + 3";
        assert_eval_eq!(text, int!(7));

        let text = "4.1 + 3.01";
        assert_eval_eq!(text, float!(7.11));

        let text = "4 + 3.01";
        assert_eval_eq!(text, float!(7.01));

        let text = "4.1 + 3";
        assert_eval_eq!(text, float!(7.1));

        let text = "-4 + 3";
        assert_eval_eq!(text, int!(-1));

        let text = "4.1 + 'a'";
        let e = eval_expect_error(text);
        assert_type_error!(e, "Unsupported operand types for +");
    }

    #[test]
    fn binary_subtraction() {
        let text = "4 - 3";
        assert_eval_eq!(text, int!(1));

        let text = "4.1 - 3.01";
        assert_eval_eq!(text, float!(1.09));

        let text = "4 - 3.01";
        assert_eval_eq!(text, float!(0.99));

        let text = "4.1 - 3";
        assert_eval_eq!(text, float!(1.1));

        let text = "-4 - 3";
        assert_eval_eq!(text, int!(-7));

        let text = "4.1 - 'a'";
        let e = eval_expect_error(text);
        assert_type_error!(e, "Unsupported operand types for -");
    }

    #[test]
    fn binary_multiplication() {
        let text = "4 * 3";
        assert_eval_eq!(text, int!(12));

        let text = "4.1 * 3.01";
        assert_eval_eq!(text, float!(12.341));

        let text = "4 * 3.01";
        assert_eval_eq!(text, float!(12.04));

        let text = "4.1 * 3";
        assert_eval_eq!(text, float!(12.3));

        let text = "-4 * 3";
        assert_eval_eq!(text, int!(-12));

        let text = "4 * 'a'";
        assert_eval_eq!(text, str!("aaaa"));

        let text = "'b' * 3";
        assert_eval_eq!(text, str!("bbb"));

        let text = "-4 * 'a'";
        assert_eval_eq!(text, str!(""));

        let text = "'b' * -3";
        assert_eval_eq!(text, str!(""));

        let text = "4.1 * 'a'";
        let e = eval_expect_error(text);
        assert_type_error!(e, "Unsupported operand types for *");
    }

    #[test]
    fn binary_division() {
        let text = "4 / 3";
        assert_eval_eq!(text, float!(1.33333333333));

        let text = "-4 / 3";
        assert_eval_eq!(text, float!(-1.33333333333));

        let text = "4.1 / 3.01";
        assert_eval_eq!(text, float!(1.36212624585));

        let text = "4 / 3.01";
        assert_eval_eq!(text, float!(1.32890365449));

        let text = "4.1 / 3";
        assert_eval_eq!(text, float!(1.36666666667));

        let text = "4.1 / 'a'";
        let e = eval_expect_error(text);
        assert_type_error!(e, "Unsupported operand types for /");
    }

    #[test]
    fn comparison_in() {
        let text = "4 in [1,2,3,4]";
        assert_eval_eq!(text, bool!(true));

        let text = "4 in [1,2,3,5]";
        assert_eval_eq!(text, bool!(false));

        let text = "not (4 in [1,2,3,5])";
        assert_eval_eq!(text, bool!(true));

        let text = r#""a" in ["a"]"#;
        assert_eval_eq!(text, bool!(true));

        let text = r#""a" in "a""#;
        let e = eval_expect_error(text);
        // TODO this shouldn't actually fail
        assert_type_error!(e, "'str' object is not iterable");
    }

    #[test]
    fn comparison_not_in() {
        let text = "4 not in [1,2,3,4]";
        assert_eval_eq!(text, bool!(false));

        let text = "4 not in [1,2,3,5]";
        assert_eval_eq!(text, bool!(true));

        let text = r#""a" not in ["a"]"#;
        assert_eval_eq!(text, bool!(false));

        let text = r#""a" not in "a""#;
        let e = eval_expect_error(text);
        // TODO this shouldn't actually fail
        assert_type_error!(e, "'str' object is not iterable");
    }

    #[test]
    fn comparison_is() {
        let text = "4 is None";
        assert_eval_eq!(text, bool!(false));
    }

    #[test]
    fn comparison_is_not() {
        let text = "4 is not None";
        assert_eval_eq!(text, bool!(true));
    }

    #[test]
    fn comparison_eq() {
        let text = "4 == 5";
        assert_eval_eq!(text, bool!(false));

        let text = "4 == 4";
        assert_eval_eq!(text, bool!(true));

        let text = "4.1 == 4.1";
        assert_eval_eq!(text, bool!(true));

        let text = "4 == 4.1";
        assert_eval_eq!(text, bool!(false));

        let text = "4 == 4.0";
        assert_eval_eq!(text, bool!(true));

        let text = "4.0 == 4";
        assert_eval_eq!(text, bool!(true));

        let text = r#""a" == "a""#;
        assert_eval_eq!(text, bool!(true));

        let text = r#""a" == "b""#;
        assert_eval_eq!(text, bool!(false));
    }

    #[test]
    fn comparison_ne() {
        let text = "4 != 5";
        assert_eval_eq!(text, bool!(true));

        let text = "4 != 4";
        assert_eval_eq!(text, bool!(false));

        let text = "4.1 != 4.1";
        assert_eval_eq!(text, bool!(false));

        let text = "4 != 4.1";
        assert_eval_eq!(text, bool!(true));

        let text = "4 != 4.0";
        assert_eval_eq!(text, bool!(false));

        let text = "4.0 != 4";
        assert_eval_eq!(text, bool!(false));

        let text = r#""a" != "a""#;
        assert_eval_eq!(text, bool!(false));

        let text = r#""a" != "b""#;
        assert_eval_eq!(text, bool!(true));
    }

    #[test]
    fn comparison_less_than() {
        let text = "4 < 5";
        assert_eval_eq!(text, bool!(true));

        let text = "6 < 5";
        assert_eval_eq!(text, bool!(false));

        let text = "5 < 5";
        assert_eval_eq!(text, bool!(false));

        let text = "4.1 < 5";
        assert_eval_eq!(text, bool!(true));

        let text = "6.1 < 5";
        assert_eval_eq!(text, bool!(false));

        let text = "4 < 5.1";
        assert_eval_eq!(text, bool!(true));

        let text = "6 < 5.1";
        assert_eval_eq!(text, bool!(false));

        let text = "4.1 < 5.1";
        assert_eval_eq!(text, bool!(true));

        let text = "6.1 < 5.1";
        assert_eval_eq!(text, bool!(false));

        let text = "6.1 < 6.1";
        assert_eval_eq!(text, bool!(false));
    }

    #[test]
    fn comparison_less_than_or_equal() {
        let text = "4 <= 5";
        assert_eval_eq!(text, bool!(true));

        let text = "6 <= 5";
        assert_eval_eq!(text, bool!(false));

        let text = "5 <= 5";
        assert_eval_eq!(text, bool!(true));

        let text = "4.1 <= 5";
        assert_eval_eq!(text, bool!(true));

        let text = "6.1 <= 5";
        assert_eval_eq!(text, bool!(false));

        let text = "4 <= 5.1";
        assert_eval_eq!(text, bool!(true));

        let text = "6 <= 5.1";
        assert_eval_eq!(text, bool!(false));

        let text = "4.1 <= 5.1";
        assert_eval_eq!(text, bool!(true));

        let text = "6.1 <= 5.1";
        assert_eval_eq!(text, bool!(false));

        let text = "6.1 <= 6.1";
        assert_eval_eq!(text, bool!(true));
    }

    #[test]
    fn comparison_greater_than() {
        let text = "4 > 5";
        assert_eval_eq!(text, bool!(false));

        let text = "6 > 5";
        assert_eval_eq!(text, bool!(true));

        let text = "5 > 5";
        assert_eval_eq!(text, bool!(false));

        let text = "4.1 > 5";
        assert_eval_eq!(text, bool!(false));

        let text = "6.1 > 5";
        assert_eval_eq!(text, bool!(true));

        let text = "4 > 5.1";
        assert_eval_eq!(text, bool!(false));

        let text = "6 > 5.1";
        assert_eval_eq!(text, bool!(true));

        let text = "4.1 > 5.1";
        assert_eval_eq!(text, bool!(false));

        let text = "6.1 > 5.1";
        assert_eval_eq!(text, bool!(true));

        let text = "6.1 > 6.1";
        assert_eval_eq!(text, bool!(false));
    }

    #[test]
    fn comparison_greater_than_or_equal() {
        let text = "4 >= 5";
        assert_eval_eq!(text, bool!(false));

        let text = "6 >= 5";
        assert_eval_eq!(text, bool!(true));

        let text = "5 >= 5";
        assert_eval_eq!(text, bool!(true));

        let text = "4.1 >= 5";
        assert_eval_eq!(text, bool!(false));

        let text = "6.1 >= 5";
        assert_eval_eq!(text, bool!(true));

        let text = "4 >= 5.1";
        assert_eval_eq!(text, bool!(false));

        let text = "6 >= 5.1";
        assert_eval_eq!(text, bool!(true));

        let text = "4.1 >= 5.1";
        assert_eval_eq!(text, bool!(false));

        let text = "6.1 >= 5.1";
        assert_eval_eq!(text, bool!(true));

        let text = "6.1 >= 6.1";
        assert_eval_eq!(text, bool!(true));
    }

    #[test]
    fn operator_chaining() {
        // Equal chains
        let input = "2 == 2 == 2";
        assert_eval_eq!(input, bool!(true));

        let input = "2 == 2 == 3";
        assert_eval_eq!(input, bool!(false));

        let input = "2 == 2 == 3 == 3";
        assert_eval_eq!(input, bool!(false));

        // Increasing chain
        let input = "1 < 2 < 3";
        assert_eval_eq!(input, bool!(true));

        let input = "1 < 2 < 2";
        assert_eval_eq!(input, bool!(false));

        let input = "1 < 3 < 2";
        assert_eval_eq!(input, bool!(false));

        let input = "1 < 2 < 3 < 4";
        assert_eval_eq!(input, bool!(true));

        // Mixed increasing / equality
        let input = "1 < 2 == 2 < 3";
        assert_eval_eq!(input, bool!(true));

        let input = "1 < 2 == 3 < 4";
        assert_eval_eq!(input, bool!(false));

        // Mixed with >= and <=
        let input = "3 >= 2 > 1";
        assert_eval_eq!(input, bool!(true));

        let input = "3 >= 2 > 2";
        assert_eval_eq!(input, bool!(false));

        let input = "2 <= 2 < 3";
        assert_eval_eq!(input, bool!(true));

        let input = "2 <= 1 < 3";
        assert_eval_eq!(input, bool!(false));

        // Mixed equality and less-than
        let input = "2 == 2 < 3";
        assert_eval_eq!(input, bool!(true));

        let input = "2 == 3 < 4";
        assert_eval_eq!(input, bool!(false));

        let input = "2 < 3 == 3";
        assert_eval_eq!(input, bool!(true));

        let input = "2 < 3 == 4";
        assert_eval_eq!(input, bool!(false));

        // Descending chain
        let input = "5 > 4 > 3 > 2";
        assert_eval_eq!(input, bool!(true));

        let input = "5 > 4 > 5";
        assert_eval_eq!(input, bool!(false));

        // With floats mixed in
        let input = "1 < 2.0 < 3";
        assert_eval_eq!(input, bool!(true));

        let input = "1.0 < 2 < 1.5";
        assert_eval_eq!(input, bool!(false));

        let input = "2.0 == 2 < 3.0";
        assert_eval_eq!(input, bool!(true));

        // Not-equals chain
        let input = "1 != 2 != 3";
        assert_eval_eq!(input, bool!(true));

        let input = "1 != 1 != 2";
        assert_eval_eq!(input, bool!(false));

        // Mix of == and !=
        let input = "1 == 1 != 2";
        assert_eval_eq!(input, bool!(true));

        let input = "1 == 2 != 3";
        assert_eval_eq!(input, bool!(false));

        // Chained in — both true
        let input = "2 in [1,2,3] in [[1,2,3],[4,5,6]]";
        assert_eval_eq!(input, bool!(true));

        let input = "2 in [1,2,3] in [[2,3,4],[4,5,6]]";
        assert_eval_eq!(input, bool!(false));

        // Chained not in
        let input = "4 not in [1,2,3] not in [[1,2,3],[4,5,6]]";
        assert_eval_eq!(input, bool!(false));

        // Mixed in / not in
        let input = "2 in [1,2,3] not in [[1,2,3],[4,5,6]]";
        assert_eval_eq!(input, bool!(false));

        // Mixed not in / in
        let input = "4 not in [1,2,3] in [[4,5,6],[7,8,9]]";
        assert_eval_eq!(input, bool!(false));
    }

    #[test]
    fn unary_expression_negative() {
        let input = "-2";
        assert_eval_eq!(input, int!(-2));

        let input = "-2.5";
        assert_eval_eq!(input, float!(-2.5));

        let input = "-(-2)";
        assert_eval_eq!(input, int!(2));

        let input = "-(-2.5)";
        assert_eval_eq!(input, float!(2.5));
    }

    #[test]
    fn binary_logical_and() {
        let input = "False and False";
        assert_eval_eq!(input, bool!(false));

        let input = "True and False";
        assert_eval_eq!(input, bool!(false));

        let input = "False and True";
        assert_eval_eq!(input, bool!(false));

        let input = "True and True";
        assert_eval_eq!(input, bool!(true));
    }

    #[test]
    fn binary_logical_or() {
        let input = "False or False";
        assert_eval_eq!(input, bool!(false));

        let input = "True or False";
        assert_eval_eq!(input, bool!(true));

        let input = "False or True";
        assert_eval_eq!(input, bool!(true));

        let input = "True or True";
        assert_eval_eq!(input, bool!(true));
    }

    #[test]
    fn unary_expression_not() {
        let input = "not True";
        assert_eval_eq!(input, bool!(false));

        let input = "not False";
        assert_eval_eq!(input, bool!(true));

        let input = "not None";
        assert_eval_eq!(input, bool!(true));

        let input = "not 1";
        assert_eval_eq!(input, bool!(false));

        let input = "not 0";
        assert_eval_eq!(input, bool!(true));

        let input = "not 1.0";
        assert_eval_eq!(input, bool!(false));

        let input = "not 0.0";
        assert_eval_eq!(input, bool!(true));

        let input = "not [1]";
        assert_eval_eq!(input, bool!(false));

        let input = "not []";
        assert_eval_eq!(input, bool!(true));
    }

    #[test]
    fn unary_expression_invert() {
        let input = "~0b1101";
        assert_eval_eq!(input, int!(-14));
    }

    #[test]
    fn list_literal() {
        let text = "[2,3]";
        assert_eval_eq!(
            text,
            VmValue::List(List::new(vec![Reference::Int(2), Reference::Int(3)]))
        );
    }

    #[test]
    fn list_literal_with_dereference() {
        let text = r#"
x = [2,"Hello"]
"#;
        let ctx = run(text);
        let list = extract!(ctx, "x", List);
        let values = list.resolved_items(ctx.interpreter().vm()).unwrap();
        assert_eq!(values, vec![int!(2), str!("Hello")]);
    }

    #[test]
    fn list_builtin() {
        let text = "list()";
        assert_eval_eq!(text, VmValue::List(List::new(vec![])));

        let text = "list([])";
        assert_eval_eq!(text, VmValue::List(List::new(vec![])));

        let text = "list([2,3])";
        assert_eval_eq!(
            text,
            VmValue::List(List::new(vec![Reference::Int(2), Reference::Int(3)]))
        );

        let text = "list((2,3))";
        assert_eval_eq!(
            text,
            VmValue::List(List::new(vec![Reference::Int(2), Reference::Int(3)]))
        );

        let text = "list(range(2))";
        assert_eval_eq!(
            text,
            VmValue::List(List::new(vec![Reference::Int(0), Reference::Int(1)]))
        );

        let text = "list(1,2)";
        let e = run_expect_error(text);
        assert_type_error!(e, "list expected at most 1 argument, got 2");
    }

    #[test]
    fn tuple_literal() {
        let text = "(2,3)";
        assert_eval_eq!(
            text,
            VmValue::Tuple(Tuple::new(vec![Reference::Int(2), Reference::Int(3)]))
        );
    }

    #[test]
    fn tuple_literal_with_dereference() {
        let text = r#"
x = (2,"Hello")
"#;
        let ctx = run(text);
        let tuple = extract!(ctx, "x", Tuple);
        let values = tuple.resolved_items(ctx.interpreter().vm()).unwrap();
        assert_eq!(values, vec![int!(2), str!("Hello")]);
    }

    #[test]
    fn dict_literal() {
        let text = r#"{ 2: 1 }"#;
        assert_eval_eq!(
            text,
            VmValue::Dict(Dict::new(vec![(Reference::Int(2), Reference::Int(1))]))
        );
    }

    #[test]
    fn dict_literal_with_dereference() {
        let text = r#"
x = {"a": 22}
"#;
        let ctx = run(text);
        let dict = extract!(ctx, "x", Dict);
        let values = dict.resolved_items(ctx.interpreter().vm()).unwrap();
        assert_eq!(values, vec![(str!("a"), int!(22))]);
    }

    #[test]
    fn tuple_builtin() {
        let text = "tuple()";
        assert_eval_eq!(text, VmValue::Tuple(Tuple::new(vec![])));

        let text = "tuple([])";
        assert_eval_eq!(text, VmValue::Tuple(Tuple::new(vec![])));

        let text = "tuple([2,3])";
        assert_eval_eq!(
            text,
            VmValue::Tuple(Tuple::new(vec![Reference::Int(2), Reference::Int(3)]))
        );

        let text = "tuple((2,3))";
        assert_eval_eq!(
            text,
            VmValue::Tuple(Tuple::new(vec![Reference::Int(2), Reference::Int(3)]))
        );

        let text = "tuple(range(2))";
        assert_eval_eq!(
            text,
            VmValue::Tuple(Tuple::new(vec![Reference::Int(0), Reference::Int(1)]))
        );

        let text = "tuple(1,2)";
        let e = run_expect_error(text);
        assert_type_error!(e, "tuple expected at most 1 argument, got 2");
    }

    #[test]
    fn range_builtin() {
        let text = "range(5)";
        assert_eval_eq!(text, VmValue::Range(Range::with_stop(5)));

        let text = "range(2, 5)";
        assert_eval_eq!(text, VmValue::Range(Range::with_start_stop(2, 5)));

        let text = "range(2, 5, 3)";
        assert_eval_eq!(text, VmValue::Range(Range::new(2, 5, 3)));

        let text = "range()";
        let e = run_expect_error(text);
        assert_type_error!(e, "range expected at least 1 argument, got 0");

        let text = "range(1,1,1,1)";
        let e = run_expect_error(text);
        assert_type_error!(e, "range expected at most 3 arguments, got 4");
    }

    #[test]
    fn assignment_int() {
        let text = r#"
a = 5 - 3
"#;
        let ctx = run(text);
        assert_read_eq!(ctx, "a", int!(2));
    }

    #[test]
    fn assignment_str() {
        let text = r#"
a = "Hello World"
"#;
        let ctx = run(text);
        assert_read_eq!(ctx, "a", str!("Hello World"));
    }

    #[test]
    fn assignment_none() {
        let text = r#"
a = 5 - 3
b = 10
c = None
"#;
        let ctx = run(text);
        assert_read_eq!(ctx, "a", int!(2));
        assert_read_eq!(ctx, "b", int!(10));
        assert_read_eq!(ctx, "c", VmValue::None);
    }

    #[test]
    fn assignment_var() {
        let text = r#"
a = 5 - 3
b = 10 + a
"#;
        let ctx = run(text);
        assert_read_eq!(ctx, "a", int!(2));
        assert_read_eq!(ctx, "b", int!(12));
    }

    #[test]
    fn while_loop() {
        let text = r#"
i = 0
n = 4
while i < n:
    i = i + 1
"#;
        let ctx = run(text);
        assert_read_eq!(ctx, "i", int!(4));
    }

    #[test]
    fn bool_builtin() {
        let input = r#"
a = bool()
b = bool(True)
c = bool(False)
d = bool([])
e = bool([1])
f = bool('')
g = bool('hello')
h = bool(0)
i = bool(5)
j = bool(())
k = bool((1))
"#;

        let ctx = run(input);

        assert_read_eq!(ctx, "a", bool!(false));
        assert_read_eq!(ctx, "b", bool!(true));
        assert_read_eq!(ctx, "c", bool!(false));
        assert_read_eq!(ctx, "d", bool!(false));
        assert_read_eq!(ctx, "e", bool!(true));
        assert_read_eq!(ctx, "f", bool!(false));
        assert_read_eq!(ctx, "g", bool!(true));
        assert_read_eq!(ctx, "h", bool!(false));
        assert_read_eq!(ctx, "i", bool!(true));
        assert_read_eq!(ctx, "j", bool!(false));
        assert_read_eq!(ctx, "k", bool!(true));
    }

    #[test]
    fn for_in_loop_list() {
        let text = r#"
s = 0
for i in [2,3,11]:
    s = s + i
"#;
        let ctx = run(text);
        assert_read_eq!(ctx, "i", int!(11));
        assert_read_eq!(ctx, "s", int!(16));
    }

    #[test]
    fn next_builtin_list() {
        let text = r#"
it = iter([1, 2, 3])
a = next(it)
b = next(it)
c = next(it)
"#;
        let ctx = run(text);
        assert_read_eq!(ctx, "a", int!(1));
        assert_read_eq!(ctx, "b", int!(2));
        assert_read_eq!(ctx, "c", int!(3));

        let text = "next([1])";
        let e = run_expect_error(text);
        assert_type_error!(e, "'list' object is not an iterator");
    }

    #[test]
    fn for_in_loop_tuple() {
        let text = r#"
s = 0
for i in (2,3,11):
    s = s + i
"#;
        let ctx = run(text);
        assert_read_eq!(ctx, "i", int!(11));
        assert_read_eq!(ctx, "s", int!(16));
    }

    #[test]
    fn next_builtin_tuple() {
        let text = r#"
it = iter((1, 2, 3))
a = next(it)
b = next(it)
c = next(it)
"#;
        let ctx = run(text);
        assert_read_eq!(ctx, "a", int!(1));
        assert_read_eq!(ctx, "b", int!(2));
        assert_read_eq!(ctx, "c", int!(3));

        let text = "next((1,))";
        let e = run_expect_error(text);
        assert_type_error!(e, "'tuple' object is not an iterator");
    }

    #[test]
    fn for_in_loop_range() {
        let text = r#"
s = 0
for i in range(2,10,2):
    s = s + i
"#;
        let ctx = run(text);
        assert_read_eq!(ctx, "i", int!(8));
        assert_read_eq!(ctx, "s", int!(20));
    }

    #[test]
    fn for_in_loop_range_with_inner_function() {
        let text = r#"
def test():
    x = 10
    for i in range(3):
        print(i, x)

test()
"#;
        // No output to check here. This test previously failed before we emitted a PopTop after a
        // function call, and before we properly split stack vs local handling inside a Frame.
        let _ = run(text);
    }

    #[test]
    fn next_builtin_range() {
        let text = "next(iter(range(5)))";
        assert_eval_eq!(text, int!(0));

        let text = "next(range(5))";
        let e = run_expect_error(text);
        assert_type_error!(e, "'range' object is not an iterator");
    }

    #[test]
    fn next_builtin_generator() {
        let text = r#"
def gen():
    yield 1
    yield 2

g = gen()
a = next(g)
b = next(g)
"#;
        let ctx = run(text);
        assert_read_eq!(ctx, "a", int!(1));
        assert_read_eq!(ctx, "b", int!(2));

        let text = r#"
def gen():
    yield 1

g = gen()
a = next(g)
b = next(g)
"#;
        let e = run_expect_error(text);
        assert_stop_iteration!(e);
    }

    #[test]
    fn for_in_loop_generator() {
        let text = r#"
def gen():
    yield 1
    yield 2

s = 11
for i in gen():
    s = s + i
"#;
        let ctx = run(text);
        assert_read_eq!(ctx, "i", int!(2));
        assert_read_eq!(ctx, "s", int!(14));
    }

    #[test]
    fn list_builtin_generator() {
        let text = r#"
def gen():
    yield 1
    yield 2

a = list(gen())
"#;
        let ctx = run(text);
        assert_read_eq!(
            ctx,
            "a",
            VmValue::List(List::new(vec![Reference::Int(1), Reference::Int(2)]))
        );
    }

    #[test]
    fn yield_from_list_builtin_list_iterable() {
        let text = r#"
def gen():
    yield from [1, 2, 3]

a = list(gen())
"#;

        let ctx = run(text);
        assert_read_eq!(
            ctx,
            "a",
            VmValue::List(List::new(vec![
                Reference::Int(1),
                Reference::Int(2),
                Reference::Int(3)
            ]))
        );
    }

    #[test]
    fn yield_from_list_builtin_generator_iterable() {
        let text = r#"
def subgen():
    yield 1
    yield 2
    yield 4

def gen():
    yield from subgen()

a = list(gen())
"#;

        let ctx = run(text);
        assert_read_eq!(
            ctx,
            "a",
            VmValue::List(List::new(vec![
                Reference::Int(1),
                Reference::Int(2),
                Reference::Int(4)
            ]))
        );
    }

    #[test]
    fn function_call_with_parameters() {
        let text = r#"
def foo(a, b):
    return a + b

c = foo(2, 9)
"#;
        let ctx = run(text);
        assert_read_eq!(ctx, "c", int!(11));
    }

    #[test]
    fn function_call_with_local_var() {
        let text = r#"
def foo(a, b):
    c = 9
    return a + b + c

d = foo(2, 9)
"#;
        let ctx = run(text);
        assert_read_eq!(ctx, "d", int!(20));
    }

    #[test]
    fn function_call_with_no_return() {
        let text = r#"
def hello():
    print("Hello")

def world():
    print("World")

hello()
world()
"#;
        // TODO should this do something?
        let _ = run(text);
    }

    #[test]
    fn function_call_with_nested_function() {
        let text = r#"
def foo(a, b):
    def inner(c, d):
        return c * d
    return a + b + inner(a, b)

c = foo(2, 9)
"#;
        let ctx = run(text);
        assert_read_eq!(ctx, "c", int!(29));
    }

    #[test]
    fn function_call_with_callee() {
        let text = r#"
def test_decorator(func):
    def wrapper():
        return func() * 2.5
    return wrapper

def get_val_undecorated():
    return 2

a = test_decorator(get_val_undecorated)()
"#;
        let ctx = run(text);
        assert_read_eq!(ctx, "a", float!(5.0));
    }

    #[test]
    fn function_call_with_decorator() {
        let text = r#"
def test_decorator(func):
    def wrapper():
        return func() * 2
    return wrapper

@test_decorator
def once_decorated():
    return 2

b = once_decorated()
"#;
        let ctx = run(text);
        assert_read_eq!(ctx, "b", int!(4));
    }

    #[test]
    fn function_call_with_multiple_decorators() {
        let text = r#"
def test_decorator(func):
    def wrapper():
        return func() * 2
    return wrapper

@test_decorator
@test_decorator
def twice_decorated():
    return 2

c = twice_decorated()
"#;
        let ctx = run(text);
        assert_read_eq!(ctx, "c", int!(8));
    }

    #[test]
    fn function_call_with_two_stage_decorators() {
        let input = r#"
def multiply(factor):
    def decorator(func):
        def wrapper():
            return func() * factor
        return wrapper
    return decorator

@multiply(3)
def get_val():
    return 2

@multiply(4)
def get_larger_val():
    return 2

a = get_val()
b = get_larger_val()
"#;
        let ctx = run(input);

        assert_read_eq!(ctx, "a", int!(6));
        assert_read_eq!(ctx, "b", int!(8));
    }

    #[test]
    fn closures() {
        let text = r#"
def make_adder(x):
    def inner_adder(y):
        return x + y
    return inner_adder

adder = make_adder(10)
a = adder(5)
"#;
        let ctx = run(text);
        assert_read_eq!(ctx, "a", int!(15));
    }

    #[test]
    fn class_definition() {
        let text = r#"
class Foo:
    def bar():
        return 4
"#;
        let ctx = run(text);
        let class = extract!(ctx, "Foo", Class);
        assert_eq!(class.name(), "Foo");
    }

    #[test]
    fn class_instantiation() {
        let text = r#"
class Foo:
    def bar():
        return 4

f = Foo()
"#;
        let ctx = run(text);
        let _ = extract!(ctx, "f", Object);
    }

    #[test]
    fn class_with_method_call() {
        let text = r#"
class Foo:
    def bar(self):
        return 4

f = Foo()
b = f.bar()
"#;
        let ctx = run(text);
        assert_read_eq!(ctx, "b", int!(4));

        let text = r#"
class Foo:
    def bar(self, val):
        return 4 + val

f = Foo()
b = f.bar(11)
"#;
        let ctx = run(text);
        assert_read_eq!(ctx, "b", int!(15));
    }

    #[test]
    fn class_with_member_access() {
        let text = r#"
class Foo:
    pass

f = Foo()
f.x = 4
"#;
        let ctx = run(text);
        assert_member_eq!(ctx, "f", "x", int!(4));
    }

    #[test]
    fn class_with_bound_method() {
        let text = r#"
class Foo:
    def bar(self):
        self.x = 4

f = Foo()
f.bar()
"#;
        let ctx = run(text);
        assert_member_eq!(ctx, "f", "x", int!(4));
    }

    #[test]
    fn class_instantiation_with_constructor() {
        let text = r#"
class Foo:
    def __init__(self):
        self.x = 44

f = Foo()
"#;
        let ctx = run(text);
        assert_member_eq!(ctx, "f", "x", int!(44));
    }

    #[test]
    fn class_instantiation_with_constructor_and_args() {
        let text = r#"
class Foo:
    def __init__(self, val):
        self.x = val

f = Foo(33)
"#;
        let ctx = run(text);
        assert_member_eq!(ctx, "f", "x", int!(33));
    }

    #[test]
    fn class_instantiation_with_method_call() {
        let text = r#"
class Foo:
    def __init__(self, val):
        self.val = val

    def bar(self):
        return self.val

f = Foo(10)
b = f.bar()
"#;
        let ctx = run(text);
        assert_read_eq!(ctx, "b", int!(10));
    }

    #[test]
    fn regular_import_same_file() {
        let ctx = run_path("src/bytecode_vm/fixtures/imports/one/main.py");
        assert_read_eq!(ctx, "x", int!(5));
    }

    #[test]
    fn regular_import_function_in_other_file() {
        let ctx = run_path("src/bytecode_vm/fixtures/imports/two/main.py");
        assert_read_eq!(ctx, "x", int!(33));
    }

    #[test]
    fn selective_import_relative() {
        let ctx = run_path("src/fixtures/imports/relative/main_a.py");
        assert_read_eq!(ctx, "x", int!(2));
    }

    #[test]
    fn regular_import_relative_parent_package() {
        let ctx = run_path("src/fixtures/imports/relative/main_b.py");
        assert_read_eq!(ctx, "x", int!(2));
    }

    #[test]
    fn regular_import_relative_alias() {
        let ctx = run_path("src/fixtures/imports/relative/main_c.py");
        assert_read_eq!(ctx, "x", int!(2));
    }

    #[test]
    fn regular_import_error() {
        let text = r#"
import not_found
"#;
        let e = run_expect_error(text);
        assert_import_error!(e, "No module named not_found");
    }

    #[test]
    fn stack_trace() {
        let text = r#"
def middle_call():
    last_call()

def last_call():
    unknown()

middle_call()
"#;
        let e = run_expect_error(text);
        assert_name_error!(e, "unknown");

        let call_stack = e.debug_call_stack;
        assert_eq!(call_stack.len(), 3);
        assert_eq!(call_stack.get(0).name(), "<module>");
        assert_eq!(call_stack.get(0).file_path_str(), "<stdin>");
        assert_eq!(call_stack.get(0).line_number(), 7);
        assert_eq!(call_stack.get(1).name(), "middle_call");
        assert_eq!(call_stack.get(1).file_path_str(), "<stdin>");
        assert_eq!(call_stack.get(1).line_number(), 2);
        assert_eq!(call_stack.get(2).name(), "last_call");
        assert_eq!(call_stack.get(2).file_path_str(), "<stdin>");
        assert_eq!(call_stack.get(2).line_number(), 5);
    }

    #[test]
    fn stack_trace_from_file() {
        let e = run_path_expect_error("src/fixtures/call_stack/call_stack_one_file.py");
        assert_name_error!(e, "unknown");

        let call_stack = e.debug_call_stack;
        assert_eq!(call_stack.len(), 3);
        assert_eq!(call_stack.get(0).name(), "<module>");
        assert!(call_stack
            .get(0)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/call_stack_one_file.py"));
        assert!(call_stack.get(0).file_path_str().starts_with("/"));
        assert_eq!(call_stack.get(0).line_number(), 7);
        assert_eq!(call_stack.get(1).name(), "middle_call");
        assert!(call_stack
            .get(1)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/call_stack_one_file.py"));
        assert!(call_stack.get(1).file_path_str().starts_with("/"));
        assert_eq!(call_stack.get(1).line_number(), 2);
        assert_eq!(call_stack.get(2).name(), "last_call");
        assert!(call_stack
            .get(2)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/call_stack_one_file.py"));
        assert!(call_stack.get(2).file_path_str().starts_with("/"));
        assert_eq!(call_stack.get(2).line_number(), 5);
    }

    #[test]
    fn stack_trace_multiple_files() {
        let e = run_path_expect_error("src/fixtures/call_stack/call_stack.py");
        assert_name_error!(e, "unknown");

        let call_stack = e.debug_call_stack;
        assert_eq!(call_stack.len(), 3);
        assert_eq!(call_stack.get(0).name(), "<module>");
        assert!(call_stack
            .get(0)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/call_stack.py"));
        assert!(call_stack.get(0).file_path_str().starts_with("/"));
        assert_eq!(call_stack.get(0).line_number(), 2);
        assert_eq!(call_stack.get(1).name(), "middle_call");
        assert!(call_stack
            .get(1)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/other.py"));
        assert!(call_stack.get(1).file_path_str().starts_with("/"));
        assert_eq!(call_stack.get(1).line_number(), 2);
        assert_eq!(call_stack.get(2).name(), "last_call");
        assert!(call_stack
            .get(2)
            .file_path_str()
            .ends_with("src/fixtures/call_stack/other.py"));
        assert!(call_stack.get(2).file_path_str().starts_with("/"));
        assert_eq!(call_stack.get(2).line_number(), 5);
    }
}
