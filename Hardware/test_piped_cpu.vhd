-- testbench for 32-Bit MIPS CPU

-- entity test_piped_cpu

-- architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity test_piped_cpu is

end test_piped_cpu;

architecture behavior of test_piped_cpu is
    -- Unit Under test (UUT)
    component piped_cpu

    port
    (
        clk, reset : std_logic;
		enable, halt : std_logic;
        dump : std_logic
    );
    
    end component piped_cpu;

    -- inputs
    signal clk   : std_logic;
    signal reset : std_logic;
	signal enable: std_logic;
    signal halt  : std_logic;
    signal dump  : std_logic;

    -- outputs

begin
    -- instanziiere UUT
    uut: piped_cpu port map
    (
        clk   => clk,
        reset => reset,
		enable => enable,
        halt => halt,
        dump => dump
    );


    -- clk process
    clk_proc: process
    begin
        clk <= '0';
        wait for 10 ns;
        clk <= '1';
        wait for 10 ns;
    end process;

    -- Stimulus process
    stim_proc: process
    begin

        dump <= '0';
        halt <= '0';
		reset <= '1';
		wait for 40 ns;
		reset <= '0';
		enable <= '1';
		wait;

        -- Testwerte
        
        -- TODO: Testfaelle


    assert FALSE report "Ende des Tests erreicht.";

    end process;

end architecture behavior;
