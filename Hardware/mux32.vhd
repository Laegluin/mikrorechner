--32-bit 2 to 1 Multiplexer

--entity mux32

--architecture

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity mux32 is
port(
    SEL: in std_logic;
    A: in unsigned(bit_Width-1 downto 0);
    B: in unsigned(bit_Width-1 downto 0);
    X: out unsigned(bit_Width-1 downto 0)
);
end mux32;

architecture behavior of mux32 is
begin
    X <= A when (SEL = '1') else B;
end behavior;