(*J'ai implémenté le compilateur OCaml sans coroutines, il marche sur tous les tests qui n'utilisent pas la coroutine.*)



open Luaparser.Ast
type value = Value.t
type env = Value.env

(*Une fonction juste pour mettre des \t entre les éléments d'une liste*)
let rec intercalleTab (l : 'a list) : 'a list =
  match l with
  | t::h::q -> t::"\t"::intercalleTab(h::q)
  | l -> l

(*Fonction qui crée une liste de taille n, contenant que des v*)
let rec create_list n v = match n with
  | 0 -> []
  | n -> v::(create_list (n-1) v)

(*create_env crée un environnement de variables globales glob, 
   de variables locales loc*)
let create_env (glob : (name, value) Hashtbl.t) (loc : (name, value) Hashtbl.t list) : Value.env =
  {Value.globals = glob; Value.locals = loc }


(* Fonction auxiliaire pour créer une table d'environnement à partir de noms et
   valeurs associées. *)
let create_scope (names: string list) (values: value list) : (name, value) Hashtbl.t =
  let tbl = Hashtbl.create (List.length names) in
  let rec aux names values =
    match names, values with
    | [], _ -> () (*si la liste de valeurs est trop longue, on oublie celles de trop*)
    | n1::n, [] -> (Hashtbl.add tbl n1 Value.Nil; aux n []) (*variables non assignées prennent pour valeur nil*)
    | n1::n, v1::v -> (Hashtbl.add tbl n1 v1; aux n v)
  in aux names values;
  tbl

(* Fonctions de l'interprète, mutuellement récursives. Une fonction par
   catégorie syntaxique de l'AST. *)
(* Interprète un bloc de code *)
let rec interp_block (env : env) (blk : block) : value =
  let l = create_list (List.length blk.locals) Value.Nil in
  let env' = Value.{globals = env.globals; locals = (create_scope blk.locals l)::env.locals} in
interp_stat env' blk.body;
interp_exp env' blk.ret
(* Interprète un statement *)
and interp_stat (env : env) (stat : stat) : unit =
  match stat with
    | Nop -> ()
    | Seq (s1,s2) -> (interp_stat env s1; interp_stat env s2)
    | Assign (v,e) -> 
      (match v with 
        | Name name -> (Value.set_ident env name (interp_exp env e))
        | IndexTable (e1,e2) -> 
          (let tab = Value.as_table (interp_exp env e1) in 
          let i = Value.as_table_key (interp_exp env e2) in 
          Hashtbl.add tab i (interp_exp env e)))
    | FunctionCall fc -> (let eval = interp_funcall env fc in ())
    | WhileDoEnd (e,s) -> 
      (while (Value.as_bool (interp_exp env e)) do
       interp_stat env s 
      done)
    | If (e,s1,s2) -> 
      (if (Value.as_bool (interp_exp env e))
        then interp_stat env s1
    else interp_stat env s2
    )

(* Interprète un appel de fonction *)
and interp_funcall (env : env) (fc : functioncall) : value =
  match fc with
      | (e, largs) -> 
        ( let argList = List.map (interp_exp env) largs in 
          let fonction = Value.as_function (interp_exp env e) in 
          (match fonction with 
            | Closure (lnames, envi, block) -> 
              let env' = (create_scope lnames argList) in 
              let env2 = create_env env.globals (env'::envi.locals) in
              (interp_block env2 block)
            | Print -> (List.iter print_string ((intercalleTab (List.map Value.to_string (argList)))@["\n"])); Nil))
            
            (*let env' = (create_scope lnames argList) in 
              let envi.locals := env'::envi.locals in*)
(* env, envi
   -on évalue les expressions qui vont servir pour évaluer la fonction en ces points
   dans env, environnement autour de la fonction
   - dans la fonction, on veut env + les variables dont on a besoin pour la 
   fonction -> il faut distinguer variables locales et globales
   - donc block va être évalué grâce à env + envi *)    
        
      


(* Interprète une expression *)
and interp_exp (env : env) (e : exp) : value = match e with
        | Nil -> Value.Nil
        | False -> Value.Bool(false)
        | True -> Value.Bool(true)
        | Integer(i) -> Value.Int(i)
        | Float(f) -> Value.Float(f)
        | LiteralString(s) -> Value.String(s)
        | Var(Name x) -> Value.lookup_ident env x (* à aller chercher dans la table*)
        | Var(IndexTable (e1,e2)) ->
          (try let tbl = Value.as_table(interp_exp env e1) in
          let key = Value.as_table_key(interp_exp env e2) in
          Hashtbl.find tbl key with
          Not_found -> Nil)
        
        | FunctionCallE(e,largs) -> (interp_funcall env (e,largs))
        | FunctionDef(nameList,myBlock) -> Function (Closure(nameList,env,myBlock)) (*voir si ya pas un truc chelou avec env/variable locale <3*)
        | BinOp(op,e1,e2) -> 
              (match op with
                | Addition -> Value.add (interp_exp env e1) (interp_exp env e2)
                | Subtraction -> Value.sub (interp_exp env e1) (interp_exp env e2)
                | Multiplication -> Value.mul (interp_exp env e1) (interp_exp env e2)
                (* relational operators *)
                | Equality -> Bool(Value.equal (interp_exp env e1) (interp_exp env e2))
                | Inequality -> Value.not (Bool(Value.equal (interp_exp env e1) (interp_exp env e2)))
                | Less -> Bool(Value.lt (interp_exp env e1) (interp_exp env e2))
                | Greater -> Value.not (Bool(Value.le (interp_exp env e1) (interp_exp env e2)))
                | LessEq -> Bool(Value.le (interp_exp env e1) (interp_exp env e2))
                | GreaterEq -> Value.not (Bool(Value.lt (interp_exp env e1) (interp_exp env e2)))
                (* logical operators *)
                | LogicalAnd -> let v1 = (interp_exp env e1) in
                    (match v1 with
                    | Nil | Bool(false) -> v1
                    | _ -> (interp_exp env e2))
                | LogicalOr -> let v1 = (interp_exp env e1) in
                    (match v1 with
                    | Nil | Bool(false) -> (interp_exp env e2)
                    | _ -> v1))
        | UnOp (unop,e) -> let v = interp_exp env e in
          (match unop with
                | UnaryMinus -> Value.sub (Int 0L) v
                | Not -> Value.not v)
        | Table l -> (let rec create_hash table hash =
          (match table with
          | [] -> Value.Table hash
          | (e1,e2)::q -> let v1 = interp_exp env e1 in
                          let v2 = interp_exp env e2 in
                          Hashtbl.add hash (Value.as_table_key v1) v2;
                          create_hash q hash )
            in
            create_hash l (Hashtbl.create (List.length l)))









let run ast =
  let globals = Hashtbl.create 47 in
  Hashtbl.add globals "print" (Value.Function Print);
  let env = Value.{ globals; locals = [] } in
  ignore (interp_block env ast)
  (*Pour débuguer ^^ : ignore (print_string (Value.to_string(interp_block env ast)))*)
