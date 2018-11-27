--Register Write-Back

--entity reg_write_back

--architecture

--WB is needed for: Arithmetic & Logic Operations, Load Operation

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity reg_write_back is
port
(
    clk, rst: in std_logic;
    wb_control: in unsigned(1 downto 0); --mem wb, alu wb or no wb (maybe copy?)
    ALU_Out: in unsigned(bit_Width-1 downto 0);
    mem_out: in unsigned(bit_Width-1 downto 0);
    reg_write_on: out std_logic;
    reg_write_data: out unsigned(bit_Width-1 downto 0)
);
end reg_write_back;

architecture behavior of reg_write_back is
signal write_data : unsigned(bit_Width-1 downto 0);
signal wb_on : std_logic;

begin
    process(clk, rst, wb_control)
    begin
        case(wb_on) is
            when "10" => --Write back ALU result (Arithmetic & Logic Ops)
                write_data = ALU_Out;
                wb_on = '1';
            when "01" => --Write back data from memory (Load Operation)
                write_data = mem_Out;
                wb_on = '1';
            when others => --assume no write back (Jumps, Comparisons, Noop, etc.)
                wb_on = '0';
        end case;
    end process;

    reg_write_on <= wb_on;
    reg_write_data <= write_data;

end behavior;

