mod evaluator;
mod parser;
mod tokens;
mod type_checker;

fn main() {
    let code = "
data Nat
| 0
| Succ(Nat)

def add(Nat Nat) - (Nat)
| n 0       = n
| n Succ(m) = n m add Succ

def add2(Nat Nat) - (Nat)
|      =     add
| n    = n   add
| n m  = n m add

def mul(Nat Nat) - (Nat)
| n 0       = 0
| n Succ(m) = n m mul n add

# def swap(a b) - (b a)
# | x y = y x

def main() - (Nat)
| = 0 Succ Succ Succ 0 Succ Succ mul

# def main() - (Nat Nat)
# | = 0 0 Succ swap
";

    let tokens = tokens::Tokens::new(code);
    // for token in tokens {
    //     println!("{token:?}")
    // }
    let mut parser = parser::Parser::new(tokens);
    let mut type_checker = type_checker::TypeChecker::new();
    let top_levels = parser.top_levels().unwrap();
    type_checker.type_check(&top_levels).unwrap();
    let mut evaluator = evaluator::Evaluator::new();
    evaluator.eval_from_main(&top_levels);
}
