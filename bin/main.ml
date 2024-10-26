module CPU = Nes_emu.CPU
let () =
    let cpu = CPU.init in 
    let foo = [CPU.U8.of_int 169;CPU.U8.of_int 2; CPU.U8.of_int 3] in 
    let opc = CPU.U8.of_int 23 in 
    CPU.opcode opc cpu foo
