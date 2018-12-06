-- 32-Bit-MIPS-CPU    -- WIP --
-- no pipeline yet

-- entity cpu

-- architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity cpu is

port
(
    clk, reset          : in    std_logic;
    pc_out, alu_result  : out   signed(bit_Width-1 downto 0);
);

end entity cpu;


architecture behavior of cpu is
-- signale hier

    -- instruction-fetch-phase
    signal instr : unsigned(bit_Width-1 downto 0);
    signal if_pc : unsigned(bit_Width-1 downto 0);

    -- instruction-decode-phase
    signal id_pc : unsigned(bit_Width-1 downto 0);
    signal 

    -- execute-phase
        -- operanden decodieren

        -- sprung berechnen

    -- memory-access-phase
        -- operanden holen

        -- sprung durchfuehren

    -- write-back-phase


    begin

    pc : entity work.pc
        port map
        (
            clk             => pc_clk,
            enable          => pc_enable,
            write_en        => pc_write_en,
            reset           => pc_reset,
            jump_to         => pc_jump_to,
            pc_value        => if_pc
        );

    decoder : entity work.decoder
        port map
        (
            clk             => de_clk,
            enable          => de_enable,
            reg_write_en    => de_reg_write_en,
            pc_write_en     => de_pc_write_en,
            mem_write_en    => de_mem_write_en,
            pc_in           => de_pc_in,
            pc_out          => de_pc_out,
            instruction     => de_instruction,
            opcode          => de_opcode,
            A               => de_A,
            B               => de_B,
            C               => de_C,
            reg_imm         => de_reg_imm,
            jump_offset     => de_jump_offset,
            mem_offset      => de_mem_offset
        );

    registerbank : entity work.registerbank
        port map
        (
            clk              => clk,
            rst              => rst,
            reg_write_en     => reg_write_en,
            reg_write_addr   => reg_write_addr,
            reg_write_data   => reg_write_data,
            reg_write_addr_A => reg_write_addr_A,
            reg_write_data_A => reg_write_data_A,
            reg_write_addr_B => reg_write_addr_B,
            reg_write_data_B => reg_write_data_B
        );

    alu : entity work.alu
        port map
        (
            A               => alu_A,
            B               => alu_B,
            opcode          => alu_opcode,
            ALU_Out         => alu_ALU_Out,
            ALU_Flag        => alu_ALU_Flag
        );

end architecture behavior;
