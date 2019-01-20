--Delay Register

--entity delay_reg1

--architecture 

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity delay_reg1 is
port
(
    clk: in std_logic;
    A: in std_logic;
    B: out std_logic
);
end delay_reg1;

architecture behavior of delay_reg1 is

signal output : std_logic;

begin
    process(clk)
    begin
        
            output <= A;

    end process;

    B <= output;

end behavior;
