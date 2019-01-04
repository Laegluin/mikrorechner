--Delay Register

--entity delay_reg

--architecture 

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity delay_reg is
port
(
    clk, rst: in std_logic;
    A: in unsigned(bit_Width-1 downto 0);
    B: out unsigned(bit_Width-1 downto 0)
);
end delay_reg;

architecture behavior of delay_reg is

signal output : unsigned(bit_Width-1 downto 0);

begin
    process(clk, rst)
    begin
        if(rst='1') then
            --?
        else
            output <= A;
        end if;

    end process;

    B <= output;

end behavior;
