-- IF/ID Pipeline Stage

-- entity pipe_if_id

-- architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity pipe_if_id is

    port
    (
        -- pipeline stage control inputs
        clk           : in  std_logic;
        reset         : in  std_logic;

        -- inputs from instruction fetch stage
        instr_in      : in  unsigned(bit_Width-1 downto 0);
        pc_in         : in  unsigned(bit_Width-1 downto 0);
        -- inputs from register bank unneeded?

        -- outputs to decoder stage
        instr_out    : out unsigned(bit_Width-1 downto 0);
        pc_out       : out unsigned(bit_Width-1 downto 0)
    );

end entity pipe_if_id;

architecture behavior of pipe_if_id is

    -- signale
signal tmp_instr : unsigned(bit_Width-1 downto 0) := (others => '1');
signal tmp_pc    : unsigned(bit_Width-1 downto 0) := (others => '0');

begin
    process(clk)
    begin

        if reset = '1' then
            -- reset signals
            tmp_instr <= (others => '1');
            tmp_pc    <= (others => '0');

        elsif rising_edge(clk) then

            tmp_instr <= instr_in;
            tmp_pc    <= pc_in;

        end if;

    end process;

    instr_out <= tmp_instr;
    pc_out    <= tmp_pc;

end behavior;
