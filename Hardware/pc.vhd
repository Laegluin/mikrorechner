-- Program-Counter

-- entity pc

-- architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity pc is

    port
    (
        clk, enable  : in std_logic;
        write_en     : in std_logic;
        reset        : in std_logic;
        jump_to      : in unsigned(bit_Width-1 downto 0);

        pc_value     : out unsigned(bit_Width-1 downto 0)
    );
end entity pc;

architecture behavior of pc is

signal pc_old : unsigned(bit_Width-1 downto 0);
signal pc_new : unsigned(bit_Width-1 downto 0);

begin

    process(clk, reset)
    begin

        if reset = '1' then
            pc_old   <= (others => '0');
            pc_new   <= (others => '0');

        -- noop
        elsif rising_edge(clk) and enable = '0' then
            pc_new <= pc_old;

        -- jump
        elsif rising_edge(clk) and enable = '1' and write_en = '1' then
            pc_old <= pc_new;
            pc_new <= jump_to;

        -- no jump
        elsif rising_edge(clk) and enable = '1' and write_en = '0' then
            pc_old <= pc_new;
            pc_new <= pc_old + "00000000000000000000000000000100"; --increment by 4


        end if;

    end process;

    -- update output port value
    pc_value <= pc_new;

end architecture behavior;
