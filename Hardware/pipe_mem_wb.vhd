-- MEM/WB Pipeline Stage

-- entity pipe_mem_wb

-- architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity pipe_mem_wb is

    port
    (
        -- pipeline stage control inputs
        clk             : in std_logic;
        reset           : in std_logic;

        -- inputs from memory stage
        wb_control_in   : in unsigned(1 downto 0);
        C_data_in       : in unsigned(bit_Width-1 downto 0);
        mem_in          : in unsigned(bit_Width-1 downto 0);
        reg_imm_in      : in unsigned(bit_Width-1 downto 0);
        C_address_in    : in unsigned(adr_Width-1 downto 0);
        
        -- outputs to writeback stage
        wb_control_out  : out unsigned(1 downto 0);
        C_data_out      : out unsigned(bit_Width-1 downto 0);
        mem_out         : out unsigned(bit_Width-1 downto 0);
        reg_imm_out     : out unsigned(bit_Width-1 downto 0);
        C_address_out   : out unsigned(adr_Width-1 downto 0)   
    );
end entity pipe_mem_wb;

architecture behavior of pipe_mem_wb is

    -- signals
signal tmp_wb_control   : unsigned(1 downto 0) := (others => '0');
signal tmp_C_data       : unsigned(bit_Width-1 downto 0) := (others => '0');
signal tmp_mem          : unsigned(bit_Width-1 downto 0) := (others => '0');
signal tmp_reg_imm      : unsigned(bit_Width-1 downto 0) := (others => '0');
signal tmp_C_address    : unsigned(adr_Width-1 downto 0) := (others => '0');

begin
    process(clk)
    begin

        if reset = '1' then
            -- reset signals
            tmp_wb_control  <= (others => '0');
            tmp_C_data      <= (others => '0');
            tmp_mem         <= (others => '0');
            tmp_reg_imm     <= (others => '0');
            tmp_C_address   <= (others => '0');
        
        elsif rising_edge(clk) then

            tmp_wb_control  <= wb_control_in;
            tmp_C_data      <= C_data_in;
            --tmp_mem         <= mem_in;
            tmp_reg_imm     <= reg_imm_in;
            tmp_C_address   <= C_address_in;

        end if;

    end process;

    wb_control_out  <= tmp_wb_control;
    C_data_out      <= tmp_C_data;
    --mem_out         <= tmp_mem;
    mem_out <= mem_in; --dirty fix to timing issue, refactor if possible
    reg_imm_out     <= tmp_reg_imm;
    C_address_out   <= tmp_C_address;

end behavior;
