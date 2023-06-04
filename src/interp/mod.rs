//J'ai essayé d'implémenter le compilateur rust. J'ai pu implémenter certaines fonctionnalités nécessaires, mais mon compilateur ne valide pour l'instant aucun test. Il manque les fonctions lookup et set de l'environnement.

use self::{
    env::{Env, GEnv, LEnv},
    value::{Value, Function},
};
use crate::parser::ast::{Exp, Args, FunctionCall};
use crate::parser::ast::*;
use std::{rc::Rc, collections::HashMap};
use crate::parser::ast::Stat_;

mod env;
pub mod value;

impl Block {
    // Interprétation d'un bloc
    fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> Value<'ast> {
        let nloc=env.locals.extend(&self.locals,Vec::new().into_iter());
        let nenv= &mut Env {locals:nloc , globals: env.globals};
        self.body.interp(nenv);
        self.ret.interp(nenv)
    }
}

impl Stat_ {
    // Interprétation d'une instruction
    fn interp<'ast,'genv>(&self, env: &mut Env<'ast,'genv>) -> () {
        match self {
            Stat_::Nop => (),
            Stat_::Seq(s1,s2) => {s1.interp(env);s2.interp(env)},
            Stat_::Assign(var,e) => unimplemented!(),
            Stat_::StatFunctionCall(fc) => {let _a = fc.interp(env);},
            Stat_::WhileDoEnd(e,s) => unimplemented!(),
            Stat_::If(e,s1,s2) => unimplemented!(),
        }
    }
}
//match fc with
//      | (e, largs) -> 
  //      ( let argList = List.map (interp_exp env) largs in 
    //      let fonction = Value.as_function (interp_exp env e) in 
      //    (match fonction with 
        //    | Closure (lnames, envi, block) -> 
          //    let env' = (create_scope lnames argList) in 
            //  let env2 = create_env env.globals (env'::envi.locals) in
              //(interp_block env2 block)
            //| Print -> (List.iter print_string ((intercalleTab (List.map Value.to_string (argList)))@["\n"])); Nil))

impl FunctionCall {
    // Interprétation d'un appel de fonction
    fn interp<'ast,'genv>(&self, env: &mut Env<'ast,'genv>) -> Value {
        match self {
            FunctionCall(e, larg) =>{
                 let function = (e.interp(env)).as_function();
                 let arg_list: Vec<Value> = larg.iter().map(|arg| arg.interp(env)).collect();
                 match function {
                    Function::Print =>
                        {let joined_str = arg_list
                        .iter()
                        .map(|arg| Value::to_string(arg))
                        .collect::<Vec<String>>()
                        .join("\t");

                        print!("{}", joined_str);
                        Value::Nil},
                    Function::Closure(lnames,envi,block) => unimplemented!(),
                 }

                },
        }
    }
}

impl Exp_ {
    // Interprétation d'une expression
    fn interp<'ast,'genv>(&self, env: &mut Env<'ast,'genv>) -> Value {
        match self {
            Exp_::Nil => unimplemented!(),
            Exp_::False => todo!(),
            Exp_::True => todo!(),
            Exp_::Number(_) => todo!(),
            Exp_::LiteralString(_) => todo!(),
            Exp_::Var(x) => match x{
                Var::Name(nom) => todo!(),
                Var::IndexTable(_, _) => todo!(),
            },
            Exp_::ExpFunctionCall(_) => todo!(),
            Exp_::FunctionDef(_) => todo!(),
            Exp_::BinOp(_, _, _) => todo!(),
            Exp_::UnOp(_, _) => todo!(),
            Exp_::Table(_) => todo!(),
        }
    }
}

// Point d'entrée principal de l'interpréteur
pub fn run(ast: &Block) {
    let mut globals = GEnv(HashMap::new());
    let printid = "print".to_owned();
    globals.0.insert(&printid, Value::Function(Function::Print));
    let mut env = Env {
        locals: Rc::new(LEnv::Nil),
        globals: &mut globals,
    };
    ast.interp(&mut env);
}
