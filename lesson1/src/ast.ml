
(** Bril AST, with code for loading the AST from json. *)

type label = string

type var = string

type typ = TInt | TBool

type binop =
  | OAdd | OMul | OSub | ODiv
  | OEq | OLt | OGt | OLe | OGe
  | OAnd | OOr

type unop =
  | ONot

type call = {
  func : string;
  args : var list;
}

(** effect instructions *)
type einstr =
  | IJump of label
  | IBranch of { cond : string; t :  label; f : label }
  | IECall of call
  | IReturn of var option
  | IPrint of var list
  | INop

(** result instructions *)
type rinstr =
  | IConstInt of int
  | IConstBool of bool
  | IBinop of binop * var * var
  | IUnop of unop * var
  | IRCall of call
  | IId of var

type instr_or_label = [ `Label of label | `EInstr of einstr | `RInstr of var * typ * rinstr ]
type instr = [ `EInstr of einstr | `RInstr of var * typ * rinstr ]

type func = {
  name : string;
  args : (string * typ) list;
  typ : typ option;
  instrs : instr_or_label list;
}

module Load = struct
  let typ_of_json = function
    | `String "int" -> TInt
    | `String "bool" -> TBool
    | `String t -> failwith ("invalid type: " ^ t)
    | _ -> failwith "invalid type"

  let string_of_json = function
    | `String str -> str
    | _ -> failwith "expected string"

  let string_list_of_json = function
    | `List lst -> List.map string_of_json lst
    | _ -> failwith "expected list"

  let instr_of_assoc assoc =
    let op =
      List.assoc_opt "op" assoc
      |> Option.get |> string_of_json in
    let dest_opt =
      List.assoc_opt "dest" assoc
      |> Option.map string_of_json in
    let typ_opt =
      List.assoc_opt "type" assoc
      |> Option.map typ_of_json in
    let args =
      List.assoc_opt "args" assoc
      |> Option.map string_list_of_json
      |> Option.to_list |> List.flatten in
    let funcs =
      List.assoc_opt "funcs" assoc
      |> Option.map string_list_of_json
      |> Option.to_list |> List.flatten in
    let labels =
      List.assoc_opt "labels" assoc
      |> Option.map string_list_of_json
      |> Option.to_list |> List.flatten in
    let value_opt =
      List.assoc_opt "value" assoc in

    match op, dest_opt, typ_opt, args, funcs, labels, value_opt with
    | "const", Some dest, Some typ, [], [], [], Some value ->
      `RInstr (dest, typ,
               match typ, value with
               | TInt, `Int int -> IConstInt int
               | TBool, `Bool bool -> IConstBool bool
               | _ -> failwith "invalid const")

    | "add", Some dest, Some (TInt as typ), [arg1; arg2], [], [], None -> `RInstr (dest, typ, IBinop (OAdd, arg1, arg2))
    | "mul", Some dest, Some (TInt as typ), [arg1; arg2], [], [], None -> `RInstr (dest, typ, IBinop (OMul, arg1, arg2))
    | "sub", Some dest, Some (TInt as typ), [arg1; arg2], [], [], None -> `RInstr (dest, typ, IBinop (OSub, arg1, arg2))
    | "div", Some dest, Some (TInt as typ), [arg1; arg2], [], [], None -> `RInstr (dest, typ, IBinop (ODiv, arg1, arg2))

    | "eq", Some dest, Some (TBool as typ), [arg1; arg2], [], [], None -> `RInstr (dest, typ, IBinop (OEq, arg1, arg2))
    | "lt", Some dest, Some (TBool as typ), [arg1; arg2], [], [], None -> `RInstr (dest, typ, IBinop (OLt, arg1, arg2))
    | "gt", Some dest, Some (TBool as typ), [arg1; arg2], [], [], None -> `RInstr (dest, typ, IBinop (OGt, arg1, arg2))
    | "le", Some dest, Some (TBool as typ), [arg1; arg2], [], [], None -> `RInstr (dest, typ, IBinop (OLe, arg1, arg2))
    | "ge", Some dest, Some (TBool as typ), [arg1; arg2], [], [], None -> `RInstr (dest, typ, IBinop (OGe, arg1, arg2))

    | "not", Some dest, Some (TBool as typ), [arg], [], [], None -> `RInstr (dest, typ, IUnop (ONot, arg))
    | "and", Some dest, Some (TBool as typ), [arg1; arg2], [], [], None -> `RInstr (dest, typ, IBinop (OAnd, arg1, arg2))
    | "or", Some dest, Some (TBool as typ), [arg1; arg2], [], [], None -> `RInstr (dest, typ, IBinop (OOr, arg1, arg2))

    | "jmp", None, None, [], [], [label], None -> `EInstr (IJump label)
    | "br", None, None, [cond], [], [t; f], None -> `EInstr (IBranch {cond; t; f})
    | "call", None, None, args, [func], [], None -> `EInstr (IECall {func; args})
    | "call", Some dest, Some typ, args, [func], [], None -> `RInstr (dest, typ, IRCall {func; args})
    | "ret", None, None, [ret], [], [], None -> `EInstr (IReturn (Some ret))
    | "ret", None, None, [], [], [], None -> `EInstr (IReturn None)

    | "id", Some dest, Some typ, [arg], [], [], None -> `RInstr (dest, typ, IId arg)
    | "print", None, None, args, [], [], None -> `EInstr (IPrint args)
    | "nop", None, None, [], [], [], None -> `EInstr INop

    | _ -> failwith ("invalid instruction: " ^ Yojson.Basic.to_string (`Assoc assoc))

  let label_or_instr_of_json = function
    | `Assoc assoc ->
      begin
        match List.assoc_opt "label" assoc with
        | Some (`String label) -> `Label label
        | None -> instr_of_assoc assoc
        | _ -> failwith "label is not string"
      end
    | _ -> failwith "expected assoc"

  let func_arg_of_json = function
    | `Assoc assoc ->
      let name =
        match List.assoc_opt "name" assoc with
        | Some (`String name) -> name
        | _ -> failwith "missing name" in
      let typ =
        match List.assoc_opt "type" assoc with
        | Some typ -> typ_of_json typ
        | _ -> failwith "missing type" in
      name, typ
    | _ -> failwith "expected assoc"

  let func_of_json = function
    | `Assoc assoc ->
      {
        name = begin
          match List.assoc_opt "name" assoc with
          | Some (`String name) -> name
          | _ -> failwith "missing name"
        end;
        args = begin
          match List.assoc_opt "args" assoc with
          | Some (`List args) -> List.map func_arg_of_json args
          | None -> []
          | _ -> failwith "invalid args format"
        end;
        typ = begin
          match List.assoc_opt "type" assoc with
          | Some json -> Some (typ_of_json json)
          | None -> None
        end;
        instrs = begin
          match List.assoc_opt "instrs" assoc with
          | Some (`List lst) -> List.map label_or_instr_of_json lst
          | _ -> failwith "missing instrs"
        end;
      }
    | _ -> failwith "invalid function"

  let funcs_of_json = function
    | `Assoc [("functions", `List lst)] -> List.map func_of_json lst
    | _ -> failwith "missing top-level 'functions' field"
end

let load_channel channel =
  Yojson.Basic.from_channel channel |> Load.funcs_of_json
