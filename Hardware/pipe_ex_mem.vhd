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
        clk            : in std_logic;
        reset          : in std_logic;

        -- input from executer stage
        pc_wr_en_in    : in std_logic;
        mem_rw_en_in   : in unsigned(1 downto 0);
        wb_control_in  : in unsigned(1 downto 0);
        C_address_in   : in unsigned(adr_Width-1 downto 0);
        reg_imm_in     : in unsigned(bit_Width-1 downto 0);
        jump_to_in     : in unsigned(bit_Width-1 downto 0);
        mem_off_in     : in unsigned(bit_Width-1 downto 0);

        -- input from ALU
        C_data_in      : in unsigned(bit_Width-1 downto 0);

        --input from registerbank over id/ex mem
        mem_address_in : in unsigned(bit_Width-1 downto 0);

        -- output to memory access stage
        pc_wr_en_out   : out std_logic;
        mem_rw_en_out  : out unsigned(1 downto 0);
        wb_control_out : out unsigned(1 downto 0);
        C_address_out  : out unsigned(adr_Width-1 downto 0);
        reg_imm_out    : out unsigned(bit_Width-1 downto 0);
        jump_to_out    : out unsigned(bit_Width-1 downto 0);
        mem_off_out    : out unsigned(bit_Width-1 downto 0);

        C_data_out     : out unsigned(bit_Width-1 downto 0);

        mem_address_out: out unsigned(bit_Width-1 downto 0)
    );

end entity pipe_ex_mem;

architecture behavior of pipe_ex_mem is

    -- signale
signal tmp_pc_wr_en   : std_logic := '0';
signal tmp_mem_rw_en  : unsigned(1 downto 0) := (others => '0');
signal tmp_wb_control : unsigned(1 downto 0) := (others => '0');
signal tmp_C_address  : unsigned(adr_Width-1 downto 0) := (others => '0');
signal tmp_reg_imm    : unsigned(bit_Width-1 downto 0) := (others => '1');
signal tmp_jump_to    : unsigned(bit_Width-1 downto 0) := (others => '1');
signal tmp_mem_off    : unsigned(bit_Width-1 downto 0) := (others => '1');

signal tmp_C_data     : unsigned(bit_Width-1 downto 0) := (others => '0');

signal tmp_mem_address: unsigned(bit_Width-1 downto 0) := (others => '0');

begin
    process(clk)
    begin

        if reset = '1' then
            -- reset signals
            tmp_pc_wr_en   <= '0';
            tmp_mem_rw_en  <= (others => '0');
            tmp_wb_control <= (others => '0');
            tmp_C_address  <= (others => '0');
            tmp_reg_imm    <= (others => '0');
            tmp_jump_to    <= (others => '0');
            tmp_mem_off    <= (others => '0');

            tmp_C_data     <= (others => '0');

            tmp_mem_address<= (others => '0');

        elsif rising_edge(clk) then

            tmp_pc_wr_en   <= pc_wr_en_in;
            tmp_mem_rw_en  <= mem_rw_en_in;
            tmp_wb_control <= wb_control_in;
            tmp_C_address  <= C_address_in;
            tmp_reg_imm    <= reg_imm_in;
            tmp_jump_to    <= jump_to_in;
            tmp_mem_off    <= mem_off_in;

            tmp_C_data     <= C_data_in;

            tmp_mem_address<= mem_address_in;

        end if;

    end process;

    pc_wr_en_out   <= tmp_pc_wr_en;
    mem_rw_en_out  <= tmp_mem_rw_en;
    wb_control_out <= tmp_wb_control;
    C_address_out  <= tmp_C_address;
    reg_imm_out    <= tmp_reg_imm;
    jump_to_out    <= tmp_jump_to;
    mem_off_out    <= tmp_mem_off;

    C_data_out     <= tmp_C_data;

    mem_address_out<= tmp_mem_address;


end behavior;
