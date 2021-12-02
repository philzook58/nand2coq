open Assembly

type comp = Comp.t [@@deriving sexp]
type jmp = Jump.t [@@deriving sexp]
type dest = Dest.t [@@deriving sexp]