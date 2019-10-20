--Delay Register

--entity delay_reg2

--architecture 

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity delay_reg2 is
port
(
    clk, rst: in std_logic;
    A: in unsigned(1 downto 0);
    B: out unsigned(1 downto 0)
);
end delay_reg2;

architecture behavior of delay_reg2 is

signal output : unsigned(1 downto 0);

begin
    process(clk, rst)
    begin
        if(rst='1') then
            --?
        else
			if rising_edge(clk) then
            output <= A;
			end if;
        end if;

    end process;

    B <= output;

end behavior;
