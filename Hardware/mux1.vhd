--1-bit 2 to 1 Multiplexer

--entity mux1

--architecture

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity mux1 is
port(
    SEL: in std_logic;
    A: in std_logic;
    B: in std_logic;
    X: out std_logic
);
end mux1;

architecture behavior of mux1 is
begin
    X <= A when (SEL = '1') else B;
end behavior;