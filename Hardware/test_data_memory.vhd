library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity test_memory is
end test_memory;

architecture test_mem of test_memory is
    component data_memory
    port
    (
	clk, rst: in std_logic;
        mem_address: in unsigned(bit_Width-1 downto 0); --assuming 32Bit address
        mem_write_data: in unsigned(bit_Width-1 downto 0);
        mem_read_on, mem_write_on: in std_logic;
        mem_out: out unsigned(bit_Width-1 downto 0)
    );
    end component;

    signal clk, rst: std_logic;
    signal mem_address: unsigned(bit_Width-1 downto 0);
    signal mem_write_data: unsigned(bit_Width-1 downto 0);
    signal mem_read_on, mem_write_on: std_logic;
    signal mem_out: unsigned(bit_Width-1 downto 0);


    begin
	uut: data_memory port map
	(
	    clk => clk,
	    rst => rst,
	    mem_address => mem_address,
	    mem_write_data => mem_write_data,
	    mem_read_on => mem_read_on,
	    mem_write_on => mem_write_on,
	    mem_out => mem_out	
	);

	process
	begin
            clk <= '0';
            wait for 10 ns;
            clk <= '1';
            wait for 10 ns;
        end process;

	process
	begin
	    mem_read_on <= '1';
	    mem_address <= to_unsigned(0, bit_Width);
	    wait for 40 ns;

	    mem_address <= to_unsigned(1, bit_Width);
	    wait for 40 ns;

	    mem_address <= to_unsigned(2, bit_Width);
	    wait for 40 ns;

	    mem_address <= to_unsigned(3, bit_Width);
	    wait for 40 ns;

	    mem_address <= to_unsigned(4, bit_Width);
	    wait for 40 ns;

	    mem_address <= to_unsigned(5, bit_Width);
	    wait for 40 ns;

	    mem_address <= to_unsigned(6, bit_Width);
	    wait for 40 ns;

	    mem_address <= to_unsigned(7, bit_Width);
	    wait for 40 ns;

	    mem_read_on <= '0';
	    wait for 40 ns;

	    mem_address <= to_unsigned(4, bit_Width);
	    mem_write_data <= to_unsigned(9, bit_Width);
	    mem_write_on <= '1';
	    wait for 20 ns;

	    mem_write_on <= '0';
	    mem_read_on <= '1';
	    wait for 40 ns;

	    mem_address <= to_unsigned(5, bit_Width);
	    wait for 40 ns;

	    mem_address <= to_unsigned(6, bit_Width);
	    wait for 40 ns;

	    rst <= '1';
	    wait for 100 ns;
	    rst <= '0';
	    wait for 100 ns;
	end process;
end test_mem;
 

