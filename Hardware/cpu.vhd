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
    clk, sclk, reset    : in std_logic;
    enable, halt        : in std_logic
--    pc_out, alu_result  : out   unsigned(bit_Width-1 downto 0)
);

end entity cpu;


architecture behavior of cpu is
-- signale hier
    -- Namen von Blockdiagramm (+ Phase falls mehrfach auftretend)
    -- instruction-fetch-phase
    signal instruction  : unsigned(bit_Width-1 downto 0);
    signal PC_value_pc  : unsigned(bit_Width-1 downto 0); 
	signal PC_value_mem	: unsigned(bit_Width-1 downto 0);
	signal PC_value_de	: unsigned(bit_Width-1 downto 0);
    signal pc_reset     : std_logic;
    signal if_PC_enable : std_logic;
    signal manual_halt  : std_logic;
    signal manual_PC_enable     : std_logic;
    signal PC_enable    : std_logic;
    signal if_PC_write_enable   : std_logic;
    signal jump_to      : unsigned(bit_Width-1 downto 0);
    signal im_reset     : std_logic;
    --signal if_pc : unsigned(bit_Width-1 downto 0);

    -- instruction-decode-phase
    signal de_enable        : std_logic;
    signal de_reset         : std_logic;
    signal de_PC_value      : unsigned(bit_Width-1 downto 0);
    signal opcode           : unsigned(opcode_Bits-1 downto 0);
    signal jump_offset      : unsigned(bit_Width-1 downto 0);
    signal de_REG_immediate : unsigned(bit_Width-1 downto 0);
    signal de_MEM_offset    : unsigned(bit_Width-1 downto 0);
    signal de_C_address     : unsigned(adr_Width-1 downto 0);
    signal de_A_address     : unsigned(adr_Width-1 downto 0);
    signal de_B_address     : unsigned(adr_Width-1 downto 0);
    signal REG_reset        : std_logic;
    signal REG_offset_en    : std_logic;


    -- execute-phase
    signal ex_enable        : std_logic;
    signal ex_reset         : std_logic;
    signal ex_A_data        : unsigned(bit_Width-1 downto 0);
    signal ex_B_data        : unsigned(bit_Width-1 downto 0);
    signal ex_PC_enable     : std_logic; 
    signal ex_PC_write_enable : std_logic;
    signal ex_wb_control    : unsigned(1 downto 0);
    signal ex_REG_immediate : unsigned(bit_Width-1 downto 0);
    signal ex_MEM_offset    : unsigned(bit_Width-1 downto 0);
    signal MEM_rw_enable    : unsigned(1 downto 0);
    signal ex_C_address     : unsigned(adr_Width-1 downto 0);
    signal ALU_flag         : std_logic;
    signal ex_C_data        : unsigned(bit_Width-1 downto 0);
    signal ex_ALU_opcode    : unsigned(opcode_Bits-1 downto 0);
    signal ex_mem_address   : unsigned(bit_Width-1 downto 0);
    signal dr_reset         : std_logic;
    
        -- operanden decodieren

        -- sprung berechnen

    -- memory-access-phase
    signal mem_reset         : std_logic;
    signal mem_wb_control    : unsigned(1 downto 0);
    signal mem_REG_immediate : unsigned(bit_Width-1 downto 0);
    signal mem_address       : unsigned(bit_Width-1 downto 0);
    --signal mem_data          : unsigned(bit_Width-1 downto 0);
    signal mem_out           : unsigned(bit_Width-1 downto 0);
    signal ex_jump_to        : unsigned(bit_Width-1 downto 0);
        
        -- operanden holen

        -- sprung durchfuehren

    -- write-back-phase
    signal wb_reset         : std_logic;
    signal wb_C_data        : unsigned(bit_Width-1 downto 0);
    signal wb_C_address     : unsigned(adr_Width-1 downto 0);
    signal REG_write_enable : std_logic;
    signal mem_C_data       : unsigned(bit_Width-1 downto 0);
    signal mem_C_address    : unsigned(adr_Width-1 downto 0);
        
    begin

    --handle external input signals

	manual_PC_enable <= enable;
    manual_halt <= halt;
	de_enable <= enable;
    ex_enable <= enable;
    pc_reset <= reset;
    im_reset <= reset;
    ex_reset <= reset;
    REG_reset <= reset;
    mem_reset <= reset;
    dr_reset <= reset;
    wb_reset <= reset;
    de_reset <= reset;
    
    --port map: PORT IN ENTITY => SIGNAL IN CPU

    pc_en_mux : entity work.mux1
        port map
        (
            SEL => manual_halt,
            A   => manual_PC_enable,
            B   => if_PC_enable,
            X   => PC_enable
        );

    pc : entity work.pc
        port map
        (
            clk             => sclk,
            enable          => PC_enable,
            write_en        => if_PC_write_enable,
            reset           => pc_reset,
            jump_to         => jump_to,
            pc_value        => PC_value_pc
        );

	pc_value_mux : entity work.mux32OUT
		port map
		(
			A	=>	PC_value_mem,
			B	=>	PC_value_de,
			X	=>	PC_value_pc
		);

    instruction_mem : entity work.instruction_memory
        port map
        (
            clk             => clk,
            rst             => im_reset,
            mem_address     => PC_value_mem,
            mem_out         => instruction
        );

    decoder : entity work.decoder
        port map
        (
            clk             => clk,
            enable          => de_enable,
            reset           => de_reset,
--            reg_write_en    => de_reg_write_en,
--            pc_write_en     => de_pc_write_en,
--            mem_write_en    => de_mem_write_en,
            pc_in           => PC_value_de,
            pc_out          => de_PC_value,
            instruction     => instruction,
            opcode          => opcode,
            A               => de_A_address,
            B               => de_B_address,
            C               => de_C_address,      
            reg_imm         => de_REG_immediate,
            jump_offset     => jump_offset,
            mem_offset      => de_MEM_offset,
            reg_offset_en   => REG_offset_en
        );

    registerbank : entity work.registerbank
        port map
        (
            clk                 => clk,
            rst                 => REG_reset,
            reg_write_en        => REG_write_enable,
            reg_offset_en       => REG_offset_en,
            reg_write_addr      => wb_C_address,
            reg_write_data      => wb_C_data,
            reg_read_addr_A     => de_A_address,
            reg_read_data_A     => ex_A_data,
            reg_read_addr_B     => de_B_address,
            reg_read_data_B     => ex_B_data,
            reg_read_addr_C     => de_C_address,
            reg_read_data_C     => ex_mem_address
        );

    delay_reg : entity work.delay_reg
        port map
        (
            clk             => clk,
            rst             => dr_reset,
            A               => ex_mem_address,
            B               => mem_address
        );
    
    executer : entity work.executer
        port map
        (
            clk             => clk,
            enable          => ex_enable,
            reset           => ex_reset,
            pc_in           => de_PC_value,
            jump_off_in     => jump_offset,
            mem_off_in      => de_MEM_offset,
            reg_imm_in      => de_REG_immediate,
            opcode_in       => opcode,
            alu_flag        => ALU_flag,
            C_in            => de_C_address,       
            C_out           => ex_C_address,
            pc_enable       => ex_PC_enable,
            pc_write_en     => ex_PC_write_enable,
            mem_rw_en       => MEM_rw_enable,
--            reg_write_en    => ex_REG_write_enable,
            reg_imm_out     => ex_REG_immediate,
            wb_control      => ex_wb_control,
            jump_to_out     => ex_jump_to,
            mem_off_out     => ex_MEM_offset,
            opcode_out      => ex_ALU_opcode
        );

    alu : entity work.alu
        port map
        (
            A               => ex_A_data,
            B               => ex_B_data,
            opcode          => ex_ALU_opcode,
            ALU_Out         => ex_C_data,
            ALU_Flag        => ALU_Flag
        );

    data_mem : entity work.data_memory
        port map
        (
            clk                 => clk,
            rst                 => mem_reset,
            mem_address         => mem_address,
            --mem_write_data      => mem_data,
            mem_rw_en           => MEM_rw_enable,
            mem_out             => mem_out,
            mem_offset          => ex_MEM_offset,

            pc_enable_in        => ex_PC_enable,
            pc_write_enable_in  => ex_PC_write_enable,
            C_in                => ex_C_data,
            wb_control_in       => ex_wb_control,
            reg_imm_in          => ex_REG_immediate,
            jump_in             => ex_jump_to,
            jump_out            => jump_to,
            reg_imm_out         => mem_REG_immediate,
            wb_control_out      => mem_wb_control,
            C_out               => mem_C_data,
            pc_write_enable_out => if_PC_write_enable,
            pc_enable_out       => if_PC_enable, --change back to if_PC_enable
            c_address_in        => ex_C_address,
            c_address_out       => mem_C_address
        );

    writeback : entity work.reg_write_back
        port map
        (
            clk             => clk,
            rst             => wb_reset,
            wb_control      => mem_wb_control,
            ALU_Out         => mem_C_data,
            mem_out         => MEM_out,
            reg_imm         => mem_REG_immediate,
            write_back_data => wb_C_data,
            wb_address_in   => mem_C_address,
            wb_address_out  => wb_C_address,
            reg_write_en    => REG_write_enable
        );

    
end architecture behavior;
