-- Instruction-Fetcher

-- entity fetcher

-- architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity fetcher is

    port
    (
        clk, enable  : in std_logic;
        pc_value     : in unsigned(bit_Width-1 downto 0);

        instruction  : out unsigned(bit_Width-1 downto 0);
        enable_out   : out std_logic
    );
end entity fetcher;

architecture behavior of fetcher is

begin
    process(clk)
    begin

        if enable = '0' then
            enable_out <= '0';


        --TODO: pc lesen, instruction lesen, ...

        elsif rising_edge(clk) and enable = '1' then
            enable_out <= '1';

        end if;

    end process;

end behavior;
