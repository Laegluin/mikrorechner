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
        clk, sclk, reset : std_logic;
		dec_enable, pc_enable : std_logic
    );
    
    end component cpu;

    -- inputs
    signal clk   : std_logic;
    signal sclk  : std_logic;
    signal reset : std_logic;
	signal dec_enable : std_logic;
    signal pc_enable : std_logic;

    -- outputs

begin
    -- instanziiere UUT
    uut: cpu port map
    (
        clk   => clk,
        sclk  => sclk,
        reset => reset,
		dec_enable => dec_enable,
		pc_enable => pc_enable
    );


    -- clk process
    clk_proc: process
    begin
        clk <= '0';
        wait for 10 ns;
        clk <= '1';
        wait for 10 ns;
    end process;

    -- single cycle clk for pc in non-pipelined cpu
    sclk_proc: process
    begin
        sclk <= '0';
        wait for 50 ns;
        sclk <= '1';
        wait for 50 ns;
    end process;


    -- Stimulus process
    stim_proc: process
    begin

		reset <= '1';
		wait for 20 ns;
		reset <= '0';
		pc_enable <= '1';
		dec_enable <= '1';
		wait for 1000 ns;

        -- Testwerte
        
        -- TODO: Testfaelle


    assert FALSE report "Ende des Tests erreicht.";

    end process;

end architecture behavior;
