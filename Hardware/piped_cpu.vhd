-- TODO freeze pipeline on halt

-- 32-Bit-MIPS-CPU    -- WIP --
-- 5 stage pipeline implemented -- WIP, momentan bloss kopie von cpu.vhd

-- entity cpu

-- architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity piped_cpu is

port
(
    clk, reset    : in std_logic;
    enable, halt        : in std_logic
);

end entity piped_cpu;


architecture behavior of piped_cpu is
-- signale hier
    -- Namen von Blockdiagramm (+ Zielkomponente)
    -- external inputs

    signal manual_PC_enable : std_logic;
    signal manual_halt : std_logic;
	signal de_enable : std_logic;
    signal ex_enable : std_logic;
    signal pc_reset : std_logic;
    signal im_reset : std_logic;
    signal ex_reset : std_logic;
    signal REG_reset : std_logic;
    signal mem_reset : std_logic;
    signal dr_reset : std_logic;
    signal wb_reset : std_logic;
    signal de_reset : std_logic;
    signal pipeline_reset : std_logic;

    -- pc to mux

    signal PC_value_mux : unsigned(bit_Width-1 downto 0);

    -- mux to instruction mem

    signal PC_value_mem : unsigned(bit_Width-1 downto 0);

    -- mux to if/id reg

    signal PC_value_ifid : unsigned(bit_Width-1 downto 0);

	-- mux to pc

	signal PC_enable : std_logic;

    -- instruction memory to if/id reg

    signal instruction_ifid : unsigned(bit_Width-1 downto 0);

    -- if/id reg to decoder

    signal PC_value_de : unsigned(bit_Width-1 downto 0);
    signal instruction_de : unsigned(bit_Width-1 downto 0);

    -- decoder to registerbank

    signal A_address_reg : unsigned(adr_Width-1 downto 0);
    signal B_address_reg : unsigned(adr_Width-1 downto 0);
    signal C_address_reg : unsigned(adr_Width-1 downto 0);
    signal reg_offset_en : std_logic;

    -- decoder to id/ex reg
    
    signal PC_value_idex : unsigned(bit_Width-1 downto 0);
    signal opcode_idex : unsigned(opcode_Bits-1 downto 0);
    signal jump_offset_idex : unsigned(bit_Width-1 downto 0);
    signal reg_imm_idex : unsigned(bit_Width-1 downto 0);
    signal mem_offset_idex : unsigned(bit_Width-1 downto 0);

    -- registerbank to id/ex reg

    signal A_data_idex : unsigned(bit_Width-1 downto 0);
    signal B_data_idex : unsigned(bit_Width-1 downto 0);
    signal store_address_idex : unsigned(bit_Width-1 downto 0);

    -- id/ex reg to executer

    signal PC_value_ex : unsigned(bit_Width-1 downto 0);
    signal opcode_ex : unsigned(opcode_Bits-1 downto 0);
    signal jump_offset_ex : unsigned(bit_Width-1 downto 0);
    signal reg_imm_ex : unsigned(bit_Width-1 downto 0);
    signal mem_offset_ex : unsigned(bit_Width-1 downto 0);
    signal C_address_ex : unsigned(adr_Width-1 downto 0);

    -- id/ex reg to alu delay

    signal A_data_aludelay : unsigned(bit_Width-1 downto 0);
    signal B_data_aludelay : unsigned(bit_Width-1 downto 0);
    signal store_address_aludelay : unsigned(bit_Width-1 downto 0);

    -- alu delay to alu

    signal A_data_alu : unsigned(bit_Width-1 downto 0);
	signal B_data_alu : unsigned(bit_Width-1 downto 0);

    -- alu delay to ex/mem reg

    signal store_address_exmem : unsigned(bit_Width-1 downto 0);
    
    -- executer to delay reg

    signal PC_enable_exmemdelay : std_logic;
    signal PC_write_enable_exmemdelay : std_logic;
    signal wb_control_exmemdelay : unsigned(1 downto 0);
    signal jump_to_exmemdelay : unsigned(bit_Width-1 downto 0);
    signal reg_imm_exmemdelay : unsigned(bit_Width-1 downto 0);
    signal mem_offset_exmemdelay : unsigned(bit_Width-1 downto 0);
    signal mem_rw_en_exmemdelay : unsigned(1 downto 0);
    signal C_address_exmemdelay : unsigned(adr_Width-1 downto 0);

    -- executer to alu

    signal ALU_opcode : unsigned(opcode_Bits-1 downto 0);
	signal ALU_stim : std_logic; -- dummy

    -- alu to executer

    signal ALU_flag : std_logic;

    -- alu to ex/mem reg

    signal C_data_exmem : unsigned(bit_Width-1 downto 0);

    -- delay reg to ex/mem reg

    signal PC_enable_exmem : std_logic;
    signal PC_write_enable_exmem : std_logic;
    signal wb_control_exmem : unsigned(1 downto 0);
    signal jump_to_exmem : unsigned(bit_Width-1 downto 0);
    signal reg_imm_exmem : unsigned(bit_Width-1 downto 0);
    signal mem_offset_exmem : unsigned(bit_Width-1 downto 0);
    signal mem_rw_en_exmem : unsigned(1 downto 0);
    signal C_address_exmem : unsigned(adr_Width-1 downto 0);


    -- ex/mem reg to data memory

    signal PC_enable_mem : std_logic;
    signal PC_write_enable_mem : std_logic;
    signal wb_control_mem : unsigned(1 downto 0);
    signal jump_to_mem : unsigned(bit_Width-1 downto 0);
    signal reg_imm_mem : unsigned(bit_Width-1 downto 0);
    signal mem_offset_mem : unsigned(bit_Width-1 downto 0);
    signal mem_rw_en_mem : unsigned(1 downto 0);
    signal C_address_mem : unsigned(adr_Width-1 downto 0);
    signal store_address_mem : unsigned(bit_Width-1 downto 0);
    signal C_data_mem : unsigned(bit_Width-1 downto 0);

    -- data memory to pc

    signal PC_enable_mux : std_logic;
    signal PC_write_enable_pc : std_logic;
    signal jump_to_pc : unsigned(bit_Width-1 downto 0);

    -- data memory to mem/wb register

    signal wb_control_memwb : unsigned(1 downto 0);
    signal reg_imm_memwb : unsigned(bit_Width-1 downto 0);
    signal mem_out_memwb : unsigned(bit_Width-1 downto 0);
    signal C_address_memwb : unsigned(adr_Width-1 downto 0);
    signal C_data_memwb : unsigned(bit_Width-1 downto 0);

    -- mem/wb register to reg write back

    signal wb_control_wb : unsigned(1 downto 0);
    signal reg_imm_wb : unsigned(bit_Width-1 downto 0);
    signal mem_out_wb : unsigned(bit_Width-1 downto 0);
    signal C_address_wb : unsigned(adr_Width-1 downto 0);
    signal C_data_wb : unsigned(bit_Width-1 downto 0);

    -- reg write back to registerbank

    signal write_address_reg : unsigned(adr_Width-1 downto 0);
    signal write_back_data : unsigned(bit_Width-1 downto 0);
    signal reg_write_en : std_logic;

	-- testing fast halt, TODO refactor stuff accordingly
    signal pc_enable_mux2 : std_logic;
        
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
    pipeline_reset <= reset;
    
    --port map: PORT IN ENTITY => SIGNAL IN CPU

    pc_en_mux : entity work.mux1
        port map
        (
            SEL => manual_halt,
            A   => manual_PC_enable,
            B   => PC_enable_mux2,
            X   => PC_enable
        );

    pc : entity work.pc
        port map
        (
            clk             => clk,
            enable          => PC_enable,
            write_en        => PC_write_enable_pc,
            reset           => pc_reset,
            jump_to         => jump_to_pc,
            pc_value        => PC_value_mux
        );

	pc_val_mux : entity work.mux32OUT
		port map
		(
			A	=>	PC_value_mem,
			B	=>	PC_value_ifid,
			X	=>	PC_value_mux
		);

    instruction_mem : entity work.instruction_memory
        port map
        (
            clk             => clk,
            rst             => im_reset,
            mem_address     => PC_value_mem,
            mem_out         => instruction_ifid
        );

    id_if_mem : entity work.pipe_if_id
        port map
        (
            clk             => clk,
            reset           => pipeline_reset,
            instr_in        => instruction_ifid,
            pc_in           => PC_value_ifid,
            instr_out       => instruction_de,
            pc_out          => PC_value_de
        );

    decoder : entity work.decoder
        port map
        (
            clk             => clk,
            enable          => de_enable,
            reset           => de_reset,
            pc_in           => PC_value_de,
            pc_out          => PC_value_idex,
            instruction     => instruction_de,
            opcode          => opcode_idex,
            A               => A_address_reg,
            B               => B_address_reg,
            C               => C_address_reg,      
            reg_imm         => reg_imm_idex,
            jump_offset     => jump_offset_idex,
            mem_offset      => mem_offset_idex,
            reg_offset_en   => reg_offset_en
        );

    id_ex_mem : entity work.pipe_id_ex
        port map
        (
            clk             => clk,
            reset           => pipeline_reset,
            pc_in           => PC_value_idex,
            opcode_in       => opcode_idex,
			C_address_in	=> C_address_reg, --maybe refactor this, breaks convention
            reg_imm_in      => reg_imm_idex,
            jump_off_in     => jump_offset_idex,
            mem_off_in      => mem_offset_idex,
            A_data_in       => A_data_idex,
            B_data_in       => B_data_idex,
            mem_address_in  => store_address_idex,
            pc_out          => PC_value_ex,
            opcode_out      => opcode_ex,
			C_address_out	=> C_address_ex,
            reg_imm_out     => reg_imm_ex,
            jump_off_out    => jump_offset_ex,
            mem_off_out     => mem_offset_ex,
            A_data_out      => A_data_aludelay,
            B_data_out      => B_data_aludelay,
            mem_address_out => store_address_aludelay
        );

    registerbank : entity work.registerbank
        port map
        (
            clk                 => clk,
            rst                 => REG_reset,
            reg_write_en        => reg_write_en,
            reg_offset_en       => reg_offset_en,
            reg_write_addr      => write_address_reg,
            reg_write_data      => write_back_data,
            reg_read_addr_A     => A_address_reg,
            reg_read_data_A     => A_data_idex,
            reg_read_addr_B     => B_address_reg,
            reg_read_data_B     => B_data_idex,
            reg_read_addr_C     => C_address_reg,
            reg_read_data_C     => store_address_idex
        );
    
    executer : entity work.executer
        port map
        (
            clk             => clk,
            enable          => ex_enable,
            reset           => ex_reset,
            pc_in           => PC_value_ex,
            jump_off_in     => jump_offset_ex,
            mem_off_in      => mem_offset_ex,
            reg_imm_in      => reg_imm_ex,
            opcode_in       => opcode_ex,
            alu_flag        => ALU_flag,
            C_in            => C_address_ex,       
            C_out           => C_address_exmemdelay,
            pc_enable       => PC_enable_mux2, --was exmem, testing fast halt
            pc_write_en     => PC_write_enable_exmemdelay,
            mem_rw_en       => mem_rw_en_exmemdelay,
            reg_imm_out     => reg_imm_exmemdelay,
            wb_control      => wb_control_exmemdelay,
            jump_to_out     => jump_to_exmemdelay,
            mem_off_out     => mem_offset_exmemdelay,
            opcode_out      => ALU_opcode,
			alu_stim		=> ALU_stim
        );

	alu_delay_reg : entity work.alu_delay_reg
		port map
		(
			clk				=> clk,
			rst				=> pipeline_reset,
			A_in			=> A_data_aludelay,
			B_in			=> B_data_aludelay,
			C_in			=> store_address_aludelay,
			A_out			=> A_data_alu,
			B_out			=> B_data_alu,
			C_out			=> store_address_exmem			
		);

    alu : entity work.piped_alu
        port map
        (
            A               => A_data_alu,
            B               => B_data_alu,
            opcode          => ALU_opcode,
            ALU_Out         => C_data_exmem,
            ALU_Flag        => ALU_flag,
			alu_stim		=> clk
        );

    exmemdelayreg : entity work.exmem_delay_reg
        port map
        (
            clk             => clk,
            rst             => pipeline_reset,
            PC_enable_in    => PC_enable_exmemdelay,
            PC_write_enable_in  => PC_write_enable_exmemdelay,
            wb_control_in   => wb_control_exmemdelay,
            jump_to_in      => jump_to_exmemdelay,
            reg_imm_in      => reg_imm_exmemdelay,
            mem_off_in      => mem_offset_exmemdelay,
            mem_rw_en_in    => mem_rw_en_exmemdelay,
            C_addr_in       => C_address_exmemdelay,

            PC_enable_out   => PC_enable_exmem,
            PC_write_enable_out => PC_write_enable_exmem,
            wb_control_out  => wb_control_exmem,
            jump_to_out     => jump_to_exmem,
            reg_imm_out     => reg_imm_exmem,
            mem_off_out     => mem_offset_exmem,
            mem_rw_en_out   => mem_rw_en_exmem,
            C_addr_out      => C_address_exmem
        );

    ex_mem_mem : entity work.pipe_ex_mem
        port map
        (
            clk             => clk,
            reset           => pipeline_reset,
            pc_en_in        => PC_enable_exmem,
            pc_wr_en_in     => PC_write_enable_exmem,
            mem_rw_en_in    => mem_rw_en_exmem,
            wb_control_in   => wb_control_exmem,
            C_address_in    => C_address_exmem,
            reg_imm_in      => reg_imm_exmem,
            jump_to_in      => jump_to_exmem,
            mem_off_in      => mem_offset_exmem,
            C_data_in       => C_data_exmem,
            mem_address_in  => store_address_exmem,
            pc_en_out       => PC_enable_mem,
            pc_wr_en_out    => PC_write_enable_mem,
            mem_rw_en_out   => mem_rw_en_mem, -- refactor so they dont pass through mem but go straight to mem/wb reg?
            wb_control_out  => wb_control_mem,
            C_address_out   => C_address_mem,
            reg_imm_out     => reg_imm_mem,
            jump_to_out     => jump_to_mem,
            mem_off_out     => mem_offset_mem,
            C_data_out      => C_data_mem,
            mem_address_out => store_address_mem
        );

    data_mem : entity work.data_memory
        port map
        (
            clk                 => clk,
            rst                 => mem_reset,
            mem_address         => store_address_mem,
            mem_rw_en           => mem_rw_en_mem,
            mem_out             => mem_out_memwb,
            mem_offset          => mem_offset_mem,

            pc_enable_in        => PC_enable_mem,  -- refactor so they dont pass through mem but go straight to mem/wb reg?
            pc_write_enable_in  => PC_write_enable_mem,
            C_in                => C_data_mem,
            wb_control_in       => wb_control_mem,
            reg_imm_in          => reg_imm_mem,
            jump_in             => jump_to_mem,
            jump_out            => jump_to_pc,
            reg_imm_out         => reg_imm_memwb,
            wb_control_out      => wb_control_memwb,
            C_out               => C_data_memwb,
            pc_write_enable_out => PC_write_enable_pc,
            pc_enable_out       => PC_enable_mux, 
            c_address_in        => C_address_mem,
            c_address_out       => C_address_memwb
        );

    mem_wb_mem : entity work.pipe_mem_wb
        port map
        (
            clk                 => clk,
            reset               => pipeline_reset,
            wb_control_in       => wb_control_memwb,
            C_data_in           => C_data_memwb,
            mem_in              => mem_out_memwb,
            reg_imm_in          => reg_imm_memwb,
            C_address_in        => C_address_memwb,
            wb_control_out      => wb_control_wb,
            C_data_out          => C_data_wb,
            mem_out             => mem_out_wb,
            reg_imm_out         => reg_imm_wb,
            C_address_out       => C_address_wb
        );

    writeback : entity work.reg_write_back
        port map
        (
            clk             => clk,
            rst             => wb_reset,
            wb_control      => wb_control_wb,
            ALU_Out         => C_data_wb,
            mem_out         => mem_out_wb,
            reg_imm         => reg_imm_wb,
            write_back_data => write_back_data,
            wb_address_in   => C_address_wb,
            wb_address_out  => write_address_reg,
            reg_write_en    => reg_write_en
        );

    
end architecture behavior;
