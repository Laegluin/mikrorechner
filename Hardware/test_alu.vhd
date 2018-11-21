-- testbench for 32-Bit-ALU

-- entity test_ALU

-- architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_ALU is
end test_ALU;
    -- Unit Under Test (UUT)
    component ALU
    generic
    (
        bit_Width   : integer := 32; -- Wortbreite
        opcode_Bits : integer := 5  -- Opcode-Bitumfang
    );

    port
    (
        A, B        : in    unsigned(bit_Width-1 downto 0);    -- Operanden
        opcode      : in    unsigned(opcode_Width-1 downto 0); -- Opcode
        ALU_Out     : out   unsigned(bit_Width-1 downto 0);    -- Ausgang
        ALU_Flag    : out   std_logic                          -- Flag
    );
    end component;

    -- inputs
    signal A, B     : unsigned(bit_Width-1 downto 0);
    signal opcode   : unsigned(opcode_Width-1 downto 0);

    -- outputs
    signal ALU_Out  : unsigned(bit_Width-1 downto 0);
    signal ALU_Flag : std_logic;

begin
    -- instanziiere UUT
    uut: ALU port map
    (
        A => A,
        B => B,
        opcode => opcode,
        ALU_Out => ALU_Out,
        ALU_Flag => ALU_Flag
    );

    -- Stimulus process
    stim_proc: process
    begin

    -- Testwerte
    A <= "00000000000000000000000000001000" -- 8
    B <= "00000000000000000000000000000010" -- 2

    -- test addition
    opcode <= 00000;
    wait for 100 ns;
        -- expected: "00000000000000000000000000001010" = 10

    -- test subtraction
    opcode <= 10111;
    wait for 100 ns;
        -- expected: "00000000000000000000000000000110" = 6

    -- test multiplication
    opcode <= 01110;
    wait for 100 ns;
        -- expected: "00000000000000000000000000010000" = 16

    -- test division
    opcode <= 01111;
    wait for 100 ns;
        -- expected: "00000000000000000000000000000100" = 4

    -- test and
    opcode <= 10000;
    wait for 100 ns;
        -- expected: "00000000000000000000000000001010" = 10

    -- test or
    opcode <= 10001;
    wait for 100 ns;
        -- expected: "00000000000000000000000000001010" = 10

    -- test not
    opcode <= 10010;
    wait for 100 ns;
        -- expected: "11111111111111111111111111110111" = 4294967287

    -- test xor
    opcode <= 10011;
    wait for 100 ns;
        -- expected: "00000000000000000000000000001010" = 10

    -- test shiftl
    opcode <= 10100;
    wait for 100 ns;
        -- expected: "00000000000000000000000000101011" = 43

    -- test shiftr
    opcode <= 10101;
    wait for 100 ns;
        -- expected: "11000000000000000000000000000010" = 3221225474

    -- test signed_shiftr
    opcode <= 10110;
    wait for 100 ns;
        -- expected: ???



    end process;
end;
