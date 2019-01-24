--Delay Register

--entity alu_delay_reg

--architecture 

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity alu_delay_reg is
port
(
    clk, rst: in std_logic;
    A_in : in unsigned(bit_Width-1 downto 0);
	B_in : in unsigned(bit_Width-1 downto 0);
	C_in : in unsigned(bit_Width-1 downto 0);
    A_out: out unsigned(bit_Width-1 downto 0);
	B_out: out unsigned(bit_Width-1 downto 0);
	C_out: out unsigned(bit_Width-1 downto 0)
);
end alu_delay_reg;

architecture behavior of alu_delay_reg is

signal tmp_A : unsigned(bit_Width-1 downto 0);
signal tmp_B : unsigned(bit_Width-1 downto 0);
signal tmp_C : unsigned(bit_Width-1 downto 0);

begin
    process(clk, rst)
    begin
        if(rst='1') then
            --?
        else
			if rising_edge(clk) then
            	tmp_A <= A_in;
				tmp_B <= B_in;
				tmp_C <= C_in;
			end if;
        end if;

    end process;

    A_out <= tmp_A;
	B_out <= tmp_B;
	C_out <= tmp_C;

end behavior;
