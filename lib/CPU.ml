[@@@warning "-26-27"]

module U8 = Unsigned.UInt8
module U16 = Unsigned.UInt16
module U32 = Unsigned.UInt32
module U64 = Unsigned.UInt64

type t = {
    register_a : U8.t; 
    register_x : U8.t;
    status : U8.t;
    program_counter : U16.t 
}

let update_zero_and_negative_flags (cpu: t ref) (result: U8.t) = 
    if U8.equal result U8.zero then 
        cpu := {!cpu with status = U8.logor !cpu.status (U8.of_int 0b0000_0010)}
    else 
        cpu := {!cpu with status = U8.logand !cpu.status (U8.of_int 0b1111_1101)};

    
    if U8.logand result (U8.of_int 0b1000_0000) != U8.zero then 
        cpu := {!cpu with status = U8.logor !cpu.status (U8.of_int 0b1000_0000)}
    else 
        cpu := {!cpu with status = U8.logand !cpu.status (U8.of_int 0b0111_1111)};
    ()
;;

(*Load value into register_a *)
let lda_opcode (cpu: t ref) (param: U8.t)= 
            cpu := {!cpu with program_counter = U16.succ !cpu.program_counter};
            cpu := {!cpu with register_a = param};
            update_zero_and_negative_flags cpu !cpu.register_a
;;
(* Transfer value from register_a to register_x*)
let tax_opcode (cpu: t ref) = 
            cpu := {!cpu with register_x = !cpu.register_a};
            cpu := {!cpu with program_counter = U16.succ !cpu.program_counter};
            update_zero_and_negative_flags cpu !cpu.register_x
;;

(*opposite of tax instruction*)
let txa_opcode (cpu: t ref) = 
    cpu := {!cpu with register_a = !cpu.register_x};
    cpu := {!cpu with program_counter = U16.succ !cpu.program_counter};
    update_zero_and_negative_flags cpu !cpu.register_a

;;
(*Decrement register_x*)
let dex_opcode (cpu: t ref) = 
    cpu := {!cpu with register_x = U8.pred !cpu.register_x};
    cpu := {!cpu with program_counter = U16.succ !cpu.program_counter};
    update_zero_and_negative_flags cpu !cpu.register_x
;;
(*Increment register_x*)
let inx_opcode (cpu: t ref) = 
    cpu := {!cpu with register_x = U8.succ !cpu.register_x};
    cpu := {!cpu with program_counter = U16.succ !cpu.program_counter};
    update_zero_and_negative_flags cpu !cpu.register_x

let opcode (cur_opcode : U8.t) (cpu : t ref) (program_list: U8.t list) = 
    match (U8.to_int cur_opcode) with
    | 0xA9 -> 
            let param = List.nth program_list (U16.to_int !cpu.program_counter) in 
            lda_opcode cpu param;
            ()
    | 0x00 -> ()
    | 0xAA ->
            tax_opcode cpu;
           () 
    | 0x8A -> 
            txa_opcode cpu; 
            ()
    | 0xCA ->
            dex_opcode cpu;
            ()
    | 0xE8 ->
            inx_opcode cpu;
            ()
    | _ -> () 
let init : t ref = 
    ref {
        register_a = U8.of_int 0; 
        register_x = U8.of_int 0;
        status = U8.of_int 0;
        program_counter = U16.of_int 0;
    }
;;
let interpert (self: t ref) (program_list : U8.t list) = 
    self := {!self with program_counter = U16.of_int 0};
    let rec loop program_list = 
        match program_list with
            | [] -> ()
            | head::tail -> 
                    let opcode_val = head in 
                    let update_program_counter = 
                        U16.succ !self.program_counter in 
                    self := {!self with program_counter = update_program_counter};
                    opcode opcode_val self program_list;
                    loop tail
    in
    loop program_list;
    Printf.printf "foo\n"




