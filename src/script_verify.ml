open Script;;
open Address;;

module Sigver = struct
	type t = string -> string -> bool;;
end

module SStack = struct
    type t = int Stack.t;;

    let create () = Stack.create ();;

    let pop st = Stack.pop st;;

    let top st = Stack.top st;;

    let rec push_data d st = match String.length d with
    | 0 -> ()
    | n ->
        Stack.push (Char.code (String.get d 0)) st;
        push_data (String.sub d 1 (n-1)) st
    ;;

    let push v st = Stack.push v st;;
end


(*
    https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp
    https://en.bitcoin.it/wiki/Script
*)
let rec _eval sigver st altst scr =
  let rec to_endif_or_else st altst scr = match scr with
  | [] -> false
  | op::scr' -> match op with
      | OP_ENDIF -> _eval sigver st altst scr'
      | OP_ELSE -> _eval sigver st altst scr'
      | _ -> to_endif_or_else st altst scr'
  in
  let rec to_endif st altst scr = match scr with
  | [] -> false
  | op::scr' -> match op with
      | OP_ENDIF -> _eval sigver st altst scr'
      | _ -> to_endif st altst scr'
  in
  match scr with
  | [] -> (SStack.pop st) <> 0
  | op::scr' -> match op with
      (* Constants *)
      | OP_0
      | OP_FALSE -> SStack.push 0 st; _eval sigver st altst scr'
      | OP_DATA (s, data) -> SStack.push_data data st; _eval sigver st altst scr'
      | OP_PUSHDATA1 (s, data) -> SStack.push_data data st; _eval sigver st altst scr'
      | OP_PUSHDATA2 (a, b, data) -> SStack.push_data data st; _eval sigver st altst scr'
      | OP_PUSHDATA4 (a, b, c, d, data) -> SStack.push_data data st; _eval sigver st altst scr'
      | OP_1NEGATE -> SStack.push (-1) st; _eval sigver st altst scr'
      | OP_1
      | OP_TRUE -> SStack.push 1 st; _eval sigver st altst scr'
      | OP_2 -> SStack.push 2 st; _eval sigver st altst scr'
      | OP_3 -> SStack.push 3 st; _eval sigver st altst scr'
      | OP_4 -> SStack.push 4 st; _eval sigver st altst scr'
      | OP_5 -> SStack.push 5 st; _eval sigver st altst scr'
      | OP_6 -> SStack.push 6 st; _eval sigver st altst scr'
      | OP_7 -> SStack.push 7 st; _eval sigver st altst scr'
      | OP_8 -> SStack.push 8 st; _eval sigver st altst scr'
      | OP_9 -> SStack.push 9 st; _eval sigver st altst scr'
      | OP_10 -> SStack.push 10 st; _eval sigver st altst scr'
      | OP_11 -> SStack.push 11 st; _eval sigver st altst scr'
      | OP_12 -> SStack.push 12 st; _eval sigver st altst scr'
      | OP_13 -> SStack.push 13 st; _eval sigver st altst scr'
      | OP_14 -> SStack.push 14 st; _eval sigver st altst scr'
      | OP_15 -> SStack.push 15 st; _eval sigver st altst scr'
      | OP_16 -> SStack.push 16 st; _eval sigver st altst scr'

      (* Flow *)
      | OP_IF -> if SStack.pop st <> 0 then _eval sigver st altst scr' else to_endif_or_else st altst scr'
      | OP_NOTIF -> if SStack.pop st = 0 then _eval sigver st altst scr' else to_endif_or_else st altst scr'
      | OP_ELSE -> to_endif st altst scr'
      | OP_ENDIF -> _eval sigver st altst scr'
      | OP_VERIFY -> if SStack.pop st <> 0 then true else false
      | OP_RETURN (data) -> false

      (* Stack *)
      | OP_TOALTSTACK ->
          let ap = SStack.pop st in SStack.push ap altst;
          _eval sigver st altst scr'
      | OP_FROMALTSTACK ->
          let ap = SStack.pop altst in SStack.push ap st;
          _eval sigver st altst scr'
      | OP_IFDUP -> true
      | OP_DEPTH -> true
      | OP_DROP ->
          SStack.pop st |> ignore;
          _eval sigver st altst scr'
      | OP_DUP ->
          let ap = SStack.top st in SStack.push ap st;
          _eval sigver st altst scr'
      | OP_NIP -> true
      | OP_OVER -> true
      | OP_PICK -> true
      | OP_ROLL -> true
      | OP_ROT -> true
      | OP_SWAP -> true
      | OP_TUCK -> true
      | OP_2DROP -> true
      | OP_2DUP -> true
      | OP_3DUP -> true
      | OP_2OVER -> true
      | OP_2ROT -> true
      | OP_2SWAP -> true

      (* Splice *)
      | OP_SIZE -> true

      (* Bitwise logic *)
      | OP_EQUAL ->
          let a = SStack.top st in
          let b = SStack.top st in
          (if a = b then SStack.push 1 st else SStack.push 0 st);
          _eval sigver st altst scr'
      | OP_EQUALVERIFY ->
          let a = SStack.top st in
          let b = SStack.top st in
          if a = b then true else false

      (* Arithmetic*)
      | OP_1ADD ->
          let ap = SStack.top st in SStack.push (ap + 1) st;
          _eval sigver st altst scr'
      | OP_1SUB ->
          let ap = SStack.top st in SStack.push (ap - 1) st;
          _eval sigver st altst scr'
      | OP_NEGATE ->
          let ap = SStack.top st in SStack.push (-ap) st;
          _eval sigver st altst scr'
      | OP_ABS ->
          let ap = SStack.top st in SStack.push (abs ap) st;
          _eval sigver st altst scr'
      | OP_NOT ->
          let ap = SStack.top st in
          if ap = 0 then SStack.push 1 st else SStack.push 0 st;
          _eval sigver st altst scr'
      | OP_0NOTEQUAL ->
          let ap = SStack.top st in
          if ap = 0 then SStack.push 0 st else SStack.push 1 st;
          _eval sigver st altst scr'
      | OP_ADD ->
          let a = SStack.top st in
          let b = SStack.top st in
          SStack.push (a + b) st;
          _eval sigver st altst scr'
      | OP_SUB ->
          let a = SStack.top st in
          let b = SStack.top st in
          SStack.push (a - b) st;
          _eval sigver st altst scr'
      | OP_BOOLAND ->
          let a = SStack.top st in
          let b = SStack.top st in
          (if a <> 0 && b <> 0 then SStack.push 1 st else SStack.push 0 st);
          _eval sigver st altst scr'
      | OP_BOOLOR ->
          let a = SStack.top st in
          let b = SStack.top st in
          (if a <> 0 || b <> 0 then SStack.push 1 st else SStack.push 0 st);
          _eval sigver st altst scr'
      | OP_NUMEQUAL ->
          let a = SStack.top st in
          let b = SStack.top st in
          (if a = b then SStack.push 1 st else SStack.push 0 st);
          _eval sigver st altst scr'
      | OP_NUMEQUALVERIFY ->
          let a = SStack.top st in
          let b = SStack.top st in
          if a = b then true else false
      | OP_NUMNOTEQUAL ->
          let a = SStack.top st in
          let b = SStack.top st in
          (if a <> b then SStack.push 1 st else SStack.push 0 st);
          _eval sigver st altst scr'
      | OP_LESSTHAN ->
          let a = SStack.top st in
          let b = SStack.top st in
          (if a < b then SStack.push 1 st else SStack.push 0 st);
          _eval sigver st altst scr'
      | OP_GREATERTHAN ->
          let a = SStack.top st in
          let b = SStack.top st in
          (if a > b then SStack.push 1 st else SStack.push 0 st);
          _eval sigver st altst scr'
      | OP_LESSTHANOREQUAL ->
          let a = SStack.top st in
          let b = SStack.top st in
          (if a <= b then SStack.push 1 st else SStack.push 0 st);
          _eval sigver st altst scr'
      | OP_GREATERTHANOREQUAL ->
          let a = SStack.top st in
          let b = SStack.top st in
          (if a >= b then SStack.push 1 st else SStack.push 0 st);
          _eval sigver st altst scr'
      | OP_MIN ->
          let a = SStack.top st in
          let b = SStack.top st in
          (if a > b then SStack.push b st else SStack.push a st);
          _eval sigver st altst scr'
      | OP_MAX ->
          let a = SStack.top st in
          let b = SStack.top st in
          (if a > b then SStack.push a st else SStack.push b st);
          _eval sigver st altst scr'
      | OP_WITHIN ->
          let x = SStack.top st in
          let min = SStack.top st in
          let max = SStack.top st in
          (if x >= min && x < max then SStack.push 1 st else SStack.push 0 st);
          _eval sigver st altst scr'

      (* Crypto *)
      | OP_RIPEMD160 -> true
      | OP_SHA1 -> true
      | OP_SHA256 -> true
      | OP_HASH160 -> true
      | OP_HASH256 -> true
      | OP_CODESEPARATOR -> true
      | OP_CHECKSIG -> 
          let s = SStack.top st in
          let pk = SStack.top st in
          (*sigver s pk*)
          false

      | OP_CHECKSIGVERIFY -> 
          let s = SStack.top st in
          let pk = SStack.top st in
          (*SStack.push @@ sigver s pk;*)
          _eval sigver st altst scr'
          
      | OP_CHECKMULTISIG -> true
      | OP_CHECKMULTISIGVERIFY -> true

      (* Lock time *)
      | OP_CHECKLOCKTIMEVERIFY -> true
      | OP_CHECKSEQUENCEVERIFY -> true

      (* Pseudo words *)
      | OP_PUBKEYHASH -> true
      | OP_PUBKEY -> true
      | OP_INVALIDOPCODE -> true

      (* Reserved words*)
      | OP_RESERVED -> true
      | OP_VER -> true
      | OP_VERIF -> true
      | OP_VERNOTIF -> true
      | OP_RESERVED1 -> true
      | OP_RESERVED2 -> true

      | OP_NOP (x) -> _eval sigver st altst scr'
      | _ -> false
;;


let eval sigver scr =
  let st = SStack.create () in
  let altst = SStack.create () in
  try _eval sigver st altst (fst scr) with _ -> false
;;


let verify sigver s1 s2 = Script.join s1 s2 |> eval sigver;;

let is_spendable scr = not (Script_nulldata.Output.check scr);;

(* Check for common pattern: http://bitcoin.stackexchange.com/questions/35456/which-bitcoin-script-forms-should-be-detected-when-tracking-wallet-balance*)
(* https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses *)
let spendable_by src prefix =
    let ml = [
        (module Script_pubkeyhash.Output: Script_template.Output);
        (module Script_pubkey.Output: Script_template.Output);
        (module Script_scripthash.Output: Script_template.Output);
        (module Script_multisig.Output: Script_template.Output);
        (module Script_witnessscripthash.Output: Script_template.Output);
        (module Script_witnesspubkeyhash.Output: Script_template.Output)
    ] in
    let rec icheck l = match l with
    | [] -> None
    | (module M: Script_template.Output) :: ml -> 
        if M.check src then Some (M.spendable_by src prefix) else icheck ml
    in icheck ml
;;


let classify_output src =
    let ml = [
        (module Script_pubkeyhash.Output: Script_template.Output);
        (module Script_pubkey.Output: Script_template.Output);
        (module Script_scripthash.Output: Script_template.Output);
        (module Script_multisig.Output: Script_template.Output);
        (module Script_witnessscripthash.Output: Script_template.Output);
        (module Script_witnesspubkeyhash.Output: Script_template.Output)
    ] in
    let rec icheck l = match l with
    | [] -> ""
    | (module M: Script_template.Output) :: ml -> 
        if M.check src then M.name else icheck ml
    in icheck ml
;;
