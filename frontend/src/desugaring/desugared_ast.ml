module Typed = Typing.Typed_ast

type ty = Typed.ty
and ret_ty = Typed.ret_ty

type unop = Typed.unop
type binop = Typed.binop
type exp = Typed.exp
type vdecl = Typed.vdecl

type stmt = Typed.stmt
and block = Typed.block

type fdecl = Typed.fdecl
type field = Typed.field
type cdecl = { cname : Typed.id; fields : field list }
type program = Prog of fdecl list * cdecl list [@@boxed]
