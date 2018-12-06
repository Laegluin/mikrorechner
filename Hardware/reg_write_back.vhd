--Register Write-Back

--entity reg_write_back

--architecture

--WB is needed for: Arithmetic & Logic Operations, Load Operation, Copy Operation

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
    reg_read_data_A: in unsigned(bit_Width-1 downto 0);
    write_back_data: out unsigned(bit_Width-1 downto 0)
);
end reg_write_back;

architecture behavior of reg_write_back is
signal write_data : unsigned(bit_Width-1 downto 0);

begin
    process(clk, rst, wb_control)
    begin
	if(rst = '1') then
	    --nothing yet
	else
	    --if(rising_edge(clk)) then
		case(wb_control) is
		    when "11" => --Write back Register (Copy)
			write_data <= reg_read_data_A;
		    when "10" => --Write back ALU result (Arithmetic & Logic Ops)
		        write_data <= ALU_Out;
		    when "01" => --Write back data from memory (Load Operation)
		        write_data <= mem_Out;
		    when others => --Assume no write back (Jumps, Comparisons, Noop, etc.)
		end case;
	    --end if;
	end if;
    end process;

    write_back_data <= write_data;

end behavior;

