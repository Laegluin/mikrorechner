-- ID/EX Pipeline Stage

-- entity pipe_id_ex

-- architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity pipe_id_ex is

    port
    (
        -- pipeline stage control inputs
        clk            : in  std_logic;
        reset          : in  std_logic;

        -- input from decoder stage
        pc_in          : in unsigned(bit_Width-1 downto 0);
        opcode_in      : in unsigned(4 downto 0);
        A_in           : in unsigned(adr_Width-1 downto 0);
        B_in           : in unsigned(adr_Width-1 downto 0);
        C_in           : in unsigned(adr_Width-1 downto 0);
        reg_imm_in     : in unsigned(bit_Width-1 downto 0);
        jump_off_in    : in unsigned(bit_Width-1 downto 0);
        mem_off_in     : in unsigned(bit_Width-1 downto 0);
        reg_off_en_in  : in std_logic;
        alu_flag_in    : in std_logic;
        -- inputs from register bank ?

        -- output to executer stage
        pc_out         : out unsigned(bit_Width-1 downto 0);
        opcode_out     : out unsigned(4 downto 0);
        A_out          : out unsigned(adr_Width-1 downto 0);
        B_out          : out unsigned(adr_Width-1 downto 0);
        C_out          : out unsigned(adr_Width-1 downto 0);
        reg_imm_out    : out unsigned(bit_Width-1 downto 0);
        jump_off_out   : out unsigned(bit_Width-1 downto 0);
        mem_off_out    : out unsigned(bit_Width-1 downto 0);
        reg_off_en_out : out std_logic;
        alu_flag_out   : out std_logic

    );

end entity pipe_id_ex;

architecture behavior of pipe_id_ex is

    -- signale
signal tmp_pc         : unsigned(bit_Width-1 downto 0) := (others => '0');
signal tmp_opcode     : unsigned(4 downto 0) := (others => '1');
signal tmp_A          : unsigned(adr_Width-1 downto 0) := (others => '0');
signal tmp_B          : unsigned(adr_Width-1 downto 0) := (others => '0');
signal tmp_C          : unsigned(adr_Width-1 downto 0) := (others => '0');
signal tmp_reg_imm    : unsigned(bit_Width-1 downto 0) := (others => '0');
signal tmp_jump_off   : unsigned(bit_Width-1 downto 0) := (others => '0');
signal tmp_mem_off    : unsigned(bit_Width-1 downto 0) := (others => '0');
signal tmp_reg_off_en : std_logic := '0';
signal tmp_alu_flag   : std_logic := '0';

begin
    process(clk)
    begin

        if reset = '1' then
            -- reset signals
            tmp_pc         <= (others => '0');
            tmp_opcode     <= (others => '1');
            tmp_A          <= (others => '0');
            tmp_B          <= (others => '0');
            tmp_C          <= (others => '0');
            tmp_reg_imm    <= (others => '0');
            tmp_jump_off   <= (others => '0');
            tmp_mem_off    <= (others => '0');
            tmp_reg_off_en <= '0';
            tmp_alu_flag   <= '0';

        elsif rising_edge(clk) then

            tmp_pc         <= pc_in;
            tmp_opcode     <= opcode_in;
            tmp_A          <= A_in;
            tmp_B          <= B_in;
            tmp_C          <= C_in;
            tmp_reg_imm    <= reg_imm_in;
            tmp_jump_off   <= jump_off_in;
            tmp_mem_off    <= mem_off_in;
            tmp_reg_off_en <= reg_off_en_in;
            tmp_alu_flag   <= alu_flag_in;

        end if;

    end process;

    pc_out         <= tmp_pc;
    opcode_out     <= tmp_opcode;
    A_out          <= tmp_A;
    B_out          <= tmp_B;
    C_out          <= tmp_C;
    reg_imm_out    <= tmp_reg_imm;
    jump_off_out   <= tmp_jump_off;
    mem_off_out    <= tmp_mem_off;
    reg_off_en_out <= tmp_reg_off_en;
    alu_flag_out   <= tmp_alu_flag;

end behavior;
