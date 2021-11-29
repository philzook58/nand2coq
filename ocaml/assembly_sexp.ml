open Assembly

type comp = [%import: Assembly.Comp.t] [@@deriving sexp]
type jmp = [%import: Assembly.Jump.t] [@@deriving sexp]
type dest = [%import: Dest.t] [@@deriving sexp]