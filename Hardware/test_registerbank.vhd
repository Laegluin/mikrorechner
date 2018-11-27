--testbench for register file

--entity test_registerbank

--architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity test_registerbank is
end test_registerbank;

architecture test_rb of test_registerbank is
    component registerbank
    port
    (
        clk, rst: in std_logic;
        reg_write_on: in std_logic;
        reg_write_addr: in unsigned(adr_Width-1 downto 0);
        reg_write_data: in unsigned(bit_Width-1 downto 0);
        reg_read_addr_A: in unsigned(5 downto 0);
        reg_read_data_A: out unsigned(bit_Width-1 downto 0);
        reg_read_addr_B: in unsigned(5 downto 0);
        reg_read_data_B: out unsigned(bit_Width-1 downto 0)
    );
    end component;
   
    signal clk, rst: std_logic;
    signal reg_write_on: std_logic;
    signal reg_write_addr, reg_read_addr_A, reg_read_addr_B: unsigned(adr_Width-1 downto 0);
    signal reg_write_data: unsigned(bit_Width-1 downto 0);
    signal reg_read_data_A, reg_read_data_B: unsigned(bit_Width-1 downto 0);

    begin 
        uut: registerbank port map
        (
            clk => clk,
            rst => rst,
            reg_write_on => reg_write_on,
            reg_write_addr => reg_write_addr,
            reg_write_data => reg_write_data,
            reg_read_addr_A => reg_read_addr_A,
            reg_read_data_A => reg_read_data_A,
            reg_read_addr_B => reg_read_addr_B,
            reg_read_data_B => reg_read_data_B
        );

        --clk_proc: process
        process
	begin
            clk <= '0';
            wait for 10 ns;
            clk <= '1';
            wait for 10 ns;
        end process;

        --test_proc: process
	process
        begin
            reg_write_on <= '1';
            reg_write_addr <= "000100";
            reg_write_data <= to_unsigned(64, bit_Width);
            
	        wait for 100 ns;
                --register r0 sollte Wert beinhalten
            reg_write_on <= '1';
	        reg_write_addr <= "000010";
	        reg_write_data <= to_unsigned(1, bit_Width);

            reg_read_addr_A <= "000100";
            reg_read_addr_B <= "000010";
	        wait for 100 ns;
		        --sollte den Wert ausgeben
        end process;
end test_rb;
