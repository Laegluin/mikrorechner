--32-bit 2 to 1 Multiplexer

--entity mux32OUT

--architecture

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity mux32OUT is
port(
    A: out unsigned(bit_Width-1 downto 0);
    B: out unsigned(bit_Width-1 downto 0);
    X: in unsigned(bit_Width-1 downto 0)
);
end mux32OUT;

architecture behavior of mux32OUT is
begin
    A <= X;
    B <= X;
end behavior;
