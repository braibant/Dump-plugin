(*i camlp4deps: "parsing/grammar.cma" i*)
(*i camlp4use: "pa_extend.cmp" i*)

(** Ported (quickly) to Coq 8.5rc1
  Diff:
   ConstRef  moved from  Libnames to  Globnames 
   Declarations.force  changed to Mod_subst.force_constr
   Declarations.NonPolymorphicType  changed to Declarations.RegularArity (?pb in match?)
   Util.anomaly  changed to Errors.anomaly (Pp.str ...)   
*)


(* Arcane incantations  *)
let _ = Mltop.add_known_module "Dump_plugin"

let _ = Pp.msgnl (Pp.str "Loading the Output plugin")

open Tacexpr
open Tacinterp
open Declarations

let pp_constr fmt x = Pp.pp_with fmt (Printer.pr_constr x)

VERNAC COMMAND EXTEND PrintTimingProfile
 ["Output" global(cref) "as" string(file) ] ->
   [ 
     let f = Pervasives.open_out file in 
     let fmt = Format.formatter_of_out_channel f in 
     begin
       try
	 begin match Nametab.global cref with
	   | Globnames.ConstRef sp ->   
	     begin
	       let cb = Global.lookup_constant sp in
	       match cb.Declarations.const_body with
		 | Declarations.Def lc  ->
		   let (c : Term.constr) = Mod_subst.force_constr lc  in 
		   let (ty: Term.types) = 
		     match cb.Declarations.const_type with 
		       |  Declarations.RegularArity ty -> ty
		       | _ -> Errors.anomaly (Pp.str "Output work only for non-polymorphic values")
		   in
		   let (c: Term.constr) = Vnorm.cbv_vm (Global.env ()) c ty in
		   Format.fprintf fmt "%a" pp_constr c ;
		   Pervasives.close_out f
		 | _  -> Errors.anomaly (Pp.str "Output work only for global definitions")
	     end   
	 end            
       with 
	 | _ ->  Pervasives.close_out f; Errors.anomaly ( Pp.str "Output error!")
     end;

   ]
END;;

(*
Pattern matching of [Nametab.global cref] is not exhaustive,  not matched:
Globnames.(VarRef _|IndRef _|ConstructRef _)
*)