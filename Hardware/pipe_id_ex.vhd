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
        --A_address_in   : in unsigned(adr_Width-1 downto 0); these probably go to registerbank
        --B_address_in   : in unsigned(adr_Width-1 downto 0);
        --C_address_in   : in unsigned(adr_Width-1 downto 0);
        reg_imm_in     : in unsigned(bit_Width-1 downto 0);
        jump_off_in    : in unsigned(bit_Width-1 downto 0);
        mem_off_in     : in unsigned(bit_Width-1 downto 0);
        --reg_off_en_in  : in std_logic;
        --alu_flag_in    : in std_logic;

        -- input from register bank ?
        A_data_in      : in unsigned(bit_Width-1 downto 0);
        B_data_in      : in unsigned(bit_Width-1 downto 0);
        mem_address_in : in unsigned(bit_Width-1 downto 0); 

        -- output to executer stage
        pc_out         : out unsigned(bit_Width-1 downto 0);
        opcode_out     : out unsigned(4 downto 0);
        --A_address_out  : out unsigned(adr_Width-1 downto 0); these probably go to registerbank
        --B_address_out  : out unsigned(adr_Width-1 downto 0);
        --C_address_out  : out unsigned(adr_Width-1 downto 0);
        reg_imm_out    : out unsigned(bit_Width-1 downto 0);
        jump_off_out   : out unsigned(bit_Width-1 downto 0);
        mem_off_out    : out unsigned(bit_Width-1 downto 0);
        --reg_off_en_out : out std_logic;
        --alu_flag_out   : out std_logic

        -- output to alu
        A_data_out     : out unsigned(bit_Width-1 downto 0);
        B_data_out     : out unsigned(bit_Width-1 downto 0);
        mem_address_out: out unsigned(bit_Width-1 downto 0)

    );

end entity pipe_id_ex;

architecture behavior of pipe_id_ex is

    -- signale
signal tmp_pc         : unsigned(bit_Width-1 downto 0) := (others => '0');
signal tmp_opcode     : unsigned(4 downto 0) := (others => '1');
--signal tmp_A_address  : unsigned(adr_Width-1 downto 0) := (others => '0');
--signal tmp_B_address  : unsigned(adr_Width-1 downto 0) := (others => '0');
--signal tmp_C_address  : unsigned(adr_Width-1 downto 0) := (others => '0');
signal tmp_reg_imm    : unsigned(bit_Width-1 downto 0) := (others => '0');
signal tmp_jump_off   : unsigned(bit_Width-1 downto 0) := (others => '0');
signal tmp_mem_off    : unsigned(bit_Width-1 downto 0) := (others => '0');
--signal tmp_reg_off_en : std_logic := '0';
--signal tmp_alu_flag   : std_logic := '0';
signal tmp_A_data     : unsigned(bit_Width-1 downto 0) := (others => '0');
signal tmp_B_data     : unsigned(bit_Width-1 downto 0) := (others => '0');
signal tmp_mem_address: unsigned(bit_Width-1 downto 0) := (others => '0');

begin
    process(clk)
    begin

        if reset = '1' then
            -- reset signals
            tmp_pc         <= (others => '0');
            tmp_opcode     <= (others => '1');
            --tmp_A_address  <= (others => '0');
            --tmp_B_address  <= (others => '0');
            --tmp_C_address  <= (others => '0');
            tmp_reg_imm    <= (others => '0');
            tmp_jump_off   <= (others => '0');
            tmp_mem_off    <= (others => '0');
            --tmp_reg_off_en <= '0';
            --tmp_alu_flag   <= '0';
            tmp_A_data     <= (others => '0');
            tmp_B_data     <= (others => '0');
            tmp_mem_address<= (others => '0');

        elsif rising_edge(clk) then

            tmp_pc         <= pc_in;
            tmp_opcode     <= opcode_in;
            --tmp_A_address  <= A_address_in;
            --tmp_B_address  <= B_address_in;
            --tmp_C_address  <= C_address_in;
            tmp_reg_imm    <= reg_imm_in;
            tmp_jump_off   <= jump_off_in;
            tmp_mem_off    <= mem_off_in;
            --tmp_reg_off_en <= reg_off_en_in;
            --tmp_alu_flag   <= alu_flag_in;
            tmp_A_data     <= A_data_in;
            tmp_B_data     <= B_data_in;
            tmp_mem_address<= mem_address_in;

        end if;

    end process;

    pc_out         <= tmp_pc;
    opcode_out     <= tmp_opcode;
    --A_address_out  <= tmp_A_address;
    --B_address_out  <= tmp_B_address;
    --C_address_out  <= tmp_C_address;
    reg_imm_out    <= tmp_reg_imm;
    jump_off_out   <= tmp_jump_off;
    mem_off_out    <= tmp_mem_off;
    --reg_off_en_out <= tmp_reg_off_en;
    --alu_flag_out   <= tmp_alu_flag;
    A_data_out     <= tmp_A_data;
    B_data_out     <= tmp_B_data;
    mem_address_out<= tmp_mem_address;

end behavior;
