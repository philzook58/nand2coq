
module Comp =
 struct
  type t =
  | ZERO
  | ONE
  | NEGONE
  | D
  | A
  | M
  | NOTD
  | NOTA
  | NOTM
  | NEGD
  | NEGA
  | NEGM
  | INCD
  | INCA
  | INCM
  | DECD
  | DECA
  | DECM
  | ADD_DA
  | ADD_DM
  | SUB_DA
  | SUB_DM
  | SUB_AD
  | SUB_MD
  | AND_DA
  | AND_DM
  | OR_DA
  | OR_DM
 end

module Dest =
 struct
  type t =
  | NULL
  | M
  | D
  | MD
  | A
  | AM
  | AD
  | AMD
 end

module Jump =
 struct
  type t =
  | NULL
  | JGT
  | JEQ
  | JGE
  | JLT
  | JNE
  | JLE
  | JMP
 end
