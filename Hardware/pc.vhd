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

signal pc    : unsigned(bit_Width-1 downto 0);

begin

    process(clk)
    begin

        if reset = '1' then
            pc   <= (others => '0');

        elsif rising_edge(clk) then
            if enable = '1' then
                if write_en = '1' then
                    -- jump
                    pc <= jump_to;
                else
                    -- no jump
                    pc <= pc + "00000000000000000000000000000100"; --increment by 4
                end if;
                    -- else noop
            end if;

        end if;

    end process;

    -- update output port value
    pc_value <= pc;

end architecture behavior;
