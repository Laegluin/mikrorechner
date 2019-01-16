--Register Write-Back

--entity reg_write_back

--architecture

--WB is needed for: Arithmetic & Logic & Copy Operations, Load Operation, Set Operation

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity reg_write_back is
port
(
    clk, rst		: in std_logic;
    wb_control		: in unsigned(1 downto 0); --mem wb, alu wb or no wb
    ALU_Out			: in unsigned(bit_Width-1 downto 0);
    mem_out			: in unsigned(bit_Width-1 downto 0);
	reg_imm			: in unsigned(bit_Width-1 downto 0);
	wb_address_in	: in unsigned(adr_Width-1 downto 0);

	wb_address_out	: out unsigned(adr_Width-1 downto 0);
    write_back_data	: out unsigned(bit_Width-1 downto 0);
	reg_write_en	: out std_logic
);
end reg_write_back;

architecture behavior of reg_write_back is
signal write_data : unsigned(bit_Width-1 downto 0);
signal reg_write_en_sig : std_logic;

begin
    process(clk, rst, wb_control)
    begin
	if(rst = '1') then
	    --nothing yet
	else
	    --if(rising_edge(clk)) then
		case(wb_control) is
			when "11" => --Write back immediate (Set Operation)
		        write_data <= reg_imm;
				reg_write_en_sig <= '1';
		    when "10" => --Write back ALU result (Arithmetic & Logic & Copy Ops)
		        write_data <= ALU_Out;
				reg_write_en_sig <= '1';
		    when "01" => --Write back data from memory (Load Operation)
		        write_data <= mem_Out;
				reg_write_en_sig <= '1';
		    when others => --Assume no write back (Jumps, Comparisons, Noop, etc.)
				reg_write_en_sig <= '0';
		end case;
	    --end if;
	end if;
    end process;

    write_back_data <= write_data;
	wb_address_out <= wb_address_in;
	reg_write_en <= reg_write_en_sig;

end behavior;

