-- EX/MEM Pipeline Stage

-- entity pipe_ex_mem

-- architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity pipe_ex_mem is

    port
    (
        -- pipeline stage control inputs
        clk            : in  std_logic;
        reset          : in  std_logic;

        -- input from executer stage
        pc_en_in       : in std_logic;
        pc_wr_en_in    : in std_logic;
        mem_rw_en_in   : in unsigned(1 downto 0);
        wb_control_in  : in unsigned(1 downto 0);
        opcode_in      : in unsigned(opcode_Bits-1 downto 0);
        C_in           : in unsigned(adr_Width-1 downto 0);
        reg_imm_in     : in unsigned(bit_Width-1 downto 0);
        jump_to_in     : in unsigned(bit_Width-1 downto 0);
        mem_off_in     : in unsigned(bit_Width-1 downto 0);

        -- output to memory access stage
        pc_en_out      : out std_logic;
        pc_wr_en_out   : out std_logic;
        mem_rw_en_out  : out unsigned(1 downto 0);
        wb_control_out : out unsigned(1 downto 0);
        opcode_out     : out unsigned(opcode_Bits-1 downto 0);
        C_out          : out unsigned(adr_Width-1 downto 0);
        reg_imm_out    : out unsigned(bit_Width-1 downto 0);
        jump_to_out    : out unsigned(bit_Width-1 downto 0);
        mem_off_out    : out unsigned(bit_Width-1 downto 0)
    );

end entity pipe_ex_mem;

architecture behavior of pipe_ex_mem is

    -- signale
signal tmp_pc_en      : std_logic := '0';
signal tmp_pc_wr_en   : std_logic := '0';
signal tmp_mem_rw_en  : unsigned(1 downto 0) := (others => '0');
signal tmp_wb_control : unsigned(1 downto 0) := (others => '0');
signal tmp_opcode     : unsigned(opcode_Bits-1 downto 0) := (others => '1');
signal tmp_C          : unsigned(adr_Width-1 downto 0) := (others => '0');
signal tmp_reg_imm    : unsigned(bit_Width-1 downto 0) := (others => '1');
signal tmp_jump_to    : unsigned(bit_Width-1 downto 0) := (others => '1');
signal tmp_mem_off    : unsigned(bit_Width-1 downto 0) := (others => '1');

begin
    process(clk)
    begin

        if reset = '1' then
            -- reset signals
            tmp_pc_en      <= '0';
            tmp_pc_wr_en   <= '0';
            tmp_mem_rw_en  <= (others => '0');
            tmp_wb_control <= (others => '0');
            tmp_opcode     <= (others => '1');
            tmp_C          <= (others => '0');
            tmp_reg_imm    <= (others => '0');
            tmp_jump_to    <= (others => '0');
            tmp_mem_off    <= (others => '0');

        elsif rising_edge(clk) then

            tmp_pc_en      <= pc_en_in;
            tmp_pc_wr_en   <= pc_wr_en_in;
            tmp_mem_rw_en  <= mem_rw_en_in;
            tmp_wb_control <= wb_control_in;
            tmp_opcode     <= opcode_in;
            tmp_C          <= C_in;
            tmp_reg_imm    <= reg_imm_in;
            tmp_jump_to    <= jump_to_in;
            tmp_mem_off    <= mem_off_in;

        end if;

    end process;

    pc_en_out      <= tmp_pc_en;
    pc_wr_en_out   <= tmp_pc_wr_en;
    mem_rw_en_out  <= tmp_mem_rw_en;
    wb_control_out <= tmp_wb_control;
    opcode_out     <= tmp_opcode;
    C_out          <= tmp_C;
    reg_imm_out    <= tmp_reg_imm;
    jump_to_out    <= tmp_jump_to;
    mem_off_out    <= tmp_mem_off;


end behavior;
