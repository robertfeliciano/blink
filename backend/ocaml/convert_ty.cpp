#include "convert.hpp"
#include "../ast/types.hpp"
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <stdexcept>

Ty* convert_ty(value ocaml_ty) {
    int tag = Tag_val(ocaml_ty);
    switch (tag) {
        case 0: return new TBool();
        case 1: return new TInt(String_val(Field(ocaml_ty, 0)));
        case 2: return new TFloat(String_val(Field(ocaml_ty, 0)));
        case 3: {
            value ocaml_ref_ty = Field(ocaml_ty, 0);
            int ref_tag = Tag_val(ocaml_ref_ty);
            switch (ref_tag) {
                case 0: return new TRef(new TRef::RString());
                case 1: return new TRef(new TRef::RArray(convert_ty(Field(ocaml_ref_ty, 0))));
                case 2: {
                    std::vector<Ty*> argTypes;
                    value arg_list = Field(ocaml_ref_ty, 0);
                    for (int i = 0; i < Wosize_val(arg_list); ++i)
                        argTypes.push_back(convert_ty(Field(arg_list, i)));
                    Ty* ret = convert_ty(Field(ocaml_ref_ty, 1));
                    return new TRef(new TRef::RFun(argTypes, ret));
                }
                default: throw std::runtime_error("Unknown ref_ty");
            }
        }
        default: throw std::runtime_error("Unknown ty");
    }
}
