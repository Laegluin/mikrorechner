--Memory Access Unit

--entity mem_access

--architecture

--INCOMPLETE, NOT IMPLEMENTED

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity mem_access is
port
(
    clk, rst: in std_logic;
    mem_address: in unsigned(bit_Width-1 downto 0); --assuming 32Bit address
    mem_write_data: in unsigned(bit_Width-1 downto 0);
    mem_read_on, mem_write_on: in std_logic;
    mem_out: out unsigned(bit_Width-1 downto 0)
);
end mem_access;

architecture behavior of mem_access is
signal mem_read_data : unsigned(bit_Width-1 downto 0); 

begin
    process(clk, rst, mem_read_on, mem_write_on)
    begin
        if(mem_read_on = '1') then
            mem_read_data <= to_unsigned(64, mem_read_data'length); --dummy, put memory logic here
        else 
            mem_read_data <= to_unsigned(0, mem_read_data'length);
        end if;
        if(mem_write_on = '1') then --maybe on rising edge?
            memory(mem_address) <= mem_write_data; --dummy, put memory logic here
        end if;
    end process;

    mem_out <= mem_read_data;

end behavior;
