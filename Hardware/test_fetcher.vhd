-- testbench for 32-Bit-Instruction-Fetcher

-- entity test_fetcher

-- architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity test_fetcher is

end test_fetcher;

architecture behavior of test_fetcher is
    -- Unit Under Test (UUT)
    component fetcher

    port
    (
        clk, enable  : in std_logic;
        pc_value     : in unsigned(bit_Width-1 downto 0);

        instruction  : out unsigned(bit_Width-1 downto 0);
        enable_out   : out std_logic
    );

    end component fetcher;
    -- inputs
    signal clk, enable  : std_logic;
    signal pc_value     : unsigned(bit_Width-1 downto 0);

    -- outputs
    signal instruction  : unsigned(bit_Width-1 downto 0);
    signal enable_out   : std_logic;

begin
    -- instanziiere UUT
    uut: fetcher port map
    (
        clk      => clk,
        enable   => enable,
        pc_value => pc_value,

        instruction => instruction,
        enable_out  => enable_out
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

    -- Testwerte

    enable <= '1';

        -- TODO: testfaelle ueberlegen

    assert False report "Ende des Tests erreicht.";
    wait;

    end process;


end architecture behavior;

