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
	-- control signals
    clk, rst		: in std_logic;

	-- inputs
    wb_control		: in unsigned(1 downto 0);
    ALU_Out			: in unsigned(bit_Width-1 downto 0);
    mem_out			: in unsigned(bit_Width-1 downto 0);
	reg_imm			: in unsigned(bit_Width-1 downto 0);
	wb_address_in	: in unsigned(adr_Width-1 downto 0);

	-- outputs
    write_back_data	: out unsigned(bit_Width-1 downto 0);
	reg_write_en	: out std_logic;
	wb_address_out	: out unsigned(adr_Width-1 downto 0)
);
end reg_write_back;

architecture behavior of reg_write_back is

	-- signals
signal tmp_write_back_data	: unsigned(bit_Width-1 downto 0) := (others => '0');
signal tmp_reg_write_en 	: std_logic := '0';
signal tmp_wb_address		: unsigned(adr_Width-1 downto 0) := (others => '0');

begin
    process(clk, rst)
    begin
	if rst = '1' then

	    tmp_write_back_data <= (others => '0');
		tmp_reg_write_en 	<= '0';
		tmp_wb_address		<= (others => '0');

	else
	    if rising_edge(clk) then

			tmp_wb_address <= wb_address_in;

			case(wb_control) is
				when "11" => --Write back immediate (Set Operation)

					tmp_write_back_data <= reg_imm;
					tmp_reg_write_en <= '1';

				when "10" => --Write back ALU result (Arithmetic & Logic & Copy Ops)

					tmp_write_back_data <= ALU_Out;
					tmp_reg_write_en <= '1';

				when "01" => --Write back data from memory (Load Operation)

					tmp_write_back_data <= mem_Out;
					tmp_reg_write_en <= '1';

				when others => --Assume no write back (Jumps, Comparisons, Noop, etc.)

					tmp_reg_write_en <= '0';

			end case;
	    end if;

	end if;
    end process;

	-- outputs
    write_back_data <= tmp_write_back_data;
	reg_write_en 	<= tmp_reg_write_en;
	wb_address_out 	<= tmp_wb_address;

end behavior;

