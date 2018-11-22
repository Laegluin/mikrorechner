--testbench for register file

--entity test_registerbank

--architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_registerbank is
end test_registerbank;

architecture test_rb of test_registerbank is
    component registerbank
    generic
    (
        bit_width : integer := 32;
        address_width : integer := 6
    );
    port
    (
        clk, rst: in std_logic;
        reg_write_on: in std_logic;
        reg_write_addr: in std_logic_vector(address_width-1 downto 0);
        reg_write_data: in std_logic_vector(bit_width-1 downto 0);
        reg_read_addr_A: in std_logic_vector(5 downto 0);
        reg_read_data_A: out std_logic_vector(bit_width-1 downto 0);
        reg_read_addr_B: in std_logic_vector(5 downto 0);
        reg_read_data_B: out std_logic_vector(bit_width-1 downto 0)
    );
    end component;
    
    constant bit_width: integer := 32;	--generic declaration somehow not in scope here
    constant address_width: integer := 6;
   
    signal clk, rst: std_logic;
    signal reg_write_on: std_logic;
    signal reg_write_addr, reg_read_addr_A, reg_read_addr_B: std_logic_vector(address_width-1 downto 0);
    signal reg_write_data: std_logic_vector(bit_width-1 downto 0);
    signal reg_read_data_A, reg_read_data_B: std_logic_vector(bit_width-1 downto 0);

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
            reg_write_addr <= "000000";
            reg_write_data <= std_logic_vector(to_signed(64, bit_width));
            wait for 100 ns;
                --register r0 sollte Wert beinhalten
            
            reg_read_addr_A <= "000000";
            wait for 100 ns;
		--sollte den Wert ausgeben
        end process;
end test_rb;