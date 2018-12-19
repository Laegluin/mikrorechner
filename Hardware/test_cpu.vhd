-- testbench for 32-Bit MIPS CPU

-- entity test_cpu

-- architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity test_cpu is

end test_cpu;

architecture behavior of test_cpu is
    -- Unit Under test (UUT)
    component cpu

    port
    (
        clk, reset : std_logic
    );
    
    end component cpu;

    -- inputs
    signal clk   : std_logic;
    signal reset : std_logic;

    -- outputs

begin
    -- instanziiere UUT
    uut: cpu port map
    (
        clk   => clk,
        reset => reset
    );

    -- Stimulus process
    stim_proc: process
    begin

        -- Testwerte
        
        -- TODO: Testfaelle


    assert FALSE report "Ende des Tests erreicht.";

    end process;

end architecture behavior;
