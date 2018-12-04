-- testbench for 32-Bit-pc

-- entity test_pc

-- architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity test_pc is

end test_pc;

architecture behavior of test_pc is
    -- Unit Under Test (UUT)
    component pc

    port
    (
        clk, enable  : in std_logic;
        write_en     : in std_logic;
        reset        : in std_logic;
        jump_to      : in unsigned(bit_Width-1 downto 0);

        pc_value     : out unsigned(bit_Width-1 downto 0)
    );

    end component pc;

    signal clk, enable : std_logic;
    signal write_en    : std_logic;
    signal reset       : std_logic;
    signal jump_to     : unsigned(bit_Width-1 downto 0);
    signal pc_value    : unsigned(bit_Width-1 downto 0);

begin
    -- instanziiere UUT
    uut: pc port map
    (
        clk      => clk,
        enable   => enable,
        write_en => write_en,
        reset    => reset,
        jump_to  => jump_to,

        pc_value => pc_value
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

        -- reset and init
    reset <= '1';
    write_en <= '0';
    wait for 100 ns;
    reset <= '0';

        -- test increment
    enable <= '1';

    wait for 100 ns;

        -- test jump
    jump_to <= "00000000000000000000000000000001";
    write_en <= '1';
    wait for 100 ns;

        -- test jump no write_en
    jump_to <= "00000000000000000000000000000010";
    write_en <= '0';
    wait for 100 ns;

        -- test reset
    reset <= '1';
    wait for 100 ns;

        -- test NOOP
    reset <= '0';
    enable <= '0';
    wait for 100 ns;

        -- test jump and increment
    jump_to <= "00001111000011110000111100001111";
    enable <= '1';
    write_en <= '1';
    wait for 20 ns;
    write_en <= '0';
    wait for 80 ns;

    assert False report "Ende des Tests erreicht.";
    wait;

    end process;


end architecture behavior;

