
open Base

let equal_u8 x y = 
    CPU.U8.equal x y
;;
let%test_unit "test_lda_immediate_load_data_instruction" =
    let cpu = CPU.init in 
    let program_list = [CPU.U8.of_int 0xa9; CPU.U8.of_int 0x05; CPU.U8.of_int 0x00] in
    CPU.interpert cpu program_list;
    let lda_param = (List.nth_exn program_list 1) in
    [%test_eq: bool] (equal_u8 !cpu.register_a lda_param) true;

    let register_is_zero = CPU.U8.logand !cpu.status (CPU.U8.of_int 0b0000_0010) in
    [%test_eq: bool] (CPU.U8.equal register_is_zero (CPU.U8.of_int 0b00)) true;

    let register_is_neg = CPU.U8.logand !cpu.status (CPU.U8.of_int 0b1000_0000) in 
    [%test_eq: bool] (CPU.U8.equal register_is_neg (CPU.U8.of_int 0)) true;
;;
let%test_unit "test_lda_zero_flag" =

    let cpu = CPU.init in 
    let program_list = [CPU.U8.of_int 0xa9; CPU.U8.of_int 0x00; CPU.U8.of_int 0x00] in
    CPU.interpert cpu program_list;
    let cpu_status_and_zero_flag = CPU.U8.logand !cpu.status (CPU.U8.of_int 0b0000_0010) in 
    [%test_eq: bool] (equal_u8 cpu_status_and_zero_flag (CPU.U8.of_int 0b10)) true
;;

let%test_unit "test_0xaa_tax_move_a_to_x" = 
    let cpu = CPU.init in 
    let program_list = [CPU.U8.of_int 0xa9; CPU.U8.of_int 0xA1; CPU.U8.of_int 0xAA] in 
    CPU.interpert cpu program_list;
    [%test_eq: bool] (CPU.U8.equal !cpu.register_x !cpu.register_a) true;

;;

let%test_unit "test_tax_zero_flag" = 
    let cpu = CPU.init in 
    let program_list = [CPU.U8.of_int 0xa9; CPU.U8.of_int 0x00; CPU.U8.of_int 0xAA] in 
    CPU.interpert cpu program_list;
    [%test_eq: bool] (equal_u8 !cpu.register_x CPU.U8.zero) true;
    [%test_eq: bool] (equal_u8 !cpu.status (CPU.U8.of_int 0b0000_0010)) true;
;;

let%test_unit "test_txa" = 
    let cpu = CPU.init in 
    let program_list = [CPU.U8.of_int 0xa9; CPU.U8.of_int 0x5A; CPU.U8.of_int 0x8A] in 
    CPU.interpert cpu program_list;
    [%test_eq: bool] (equal_u8 CPU.U8.zero !cpu.register_x) true

;;

let%test_unit "test_dex_and_inx" = 
    let cpu = CPU.init in 
    let program_list = [CPU.U8.of_int 0xE8; CPU.U8.of_int 0xE8; CPU.U8.of_int 0xCA] in 
    CPU.interpert cpu program_list;
    [%test_eq: bool] (equal_u8 CPU.U8.one !cpu.register_x) true

let%test_unit "test_overflow_register_x" = 
    let cpu = CPU.init in 
    let program_list = [CPU.U8.of_int 0xA9; CPU.U8.of_int 0xFF; CPU.U8.of_int 0xAA; CPU.U8.of_int 0xE8] in 
    CPU.interpert cpu program_list; 
    [%test_eq: bool] (equal_u8 !cpu.register_x CPU.U8.zero) true;
    let program_list = program_list @ [CPU.U8.of_int 0xCA] in 
    CPU.interpert cpu program_list;
    [%test_eq: bool] (equal_u8 !cpu.register_x CPU.U8.max_int) true;



    


    

    
