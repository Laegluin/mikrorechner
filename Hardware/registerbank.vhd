-- 32-Bit-Register File

--entity registerfile

--architecture

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity registerbank is
port(
    clk, rst: in std_logic;
    reg_write_on: in std_logic;
    reg_write_addr: in unsigned(adr_Width-1 downto 0);
    reg_write_data: in unsigned(bit_Width-1 downto 0);
    reg_read_addr_A: in unsigned(5 downto 0);
    reg_read_data_A: out unsigned(bit_Width-1 downto 0);
    reg_read_addr_B: in unsigned(5 downto 0);
    reg_read_data_B: out unsigned(bit_Width-1 downto 0)
);
end registerbank;

architecture behavior of registerbank is
type reg_type is array (0 to (2**(adr_Width-1))-1) of unsigned(bit_Width-1 downto 0);
signal reg_array : reg_type;
signal data_a : unsigned(bit_Width-1 downto 0);
signal data_b : unsigned(bit_Width-1 downto 0);

begin
    process(clk, rst, reg_read_addr_A, reg_read_addr_B)
	variable offset_register : unsigned(bit_Width-1 downto 0) := to_unsigned(0, bit_Width); --not sure if this is smart...
    begin
        if(rst='1') then
            --maybe do something
        elsif(rising_edge(clk)) then
            if(reg_write_on='1') then
                if(reg_write_addr="100001") then
                    offset_register := reg_write_data;
                else    
                    reg_array(to_integer(unsigned(reg_write_addr(adr_Width-2 downto 0)))) <= reg_write_data;
                end if;
	    end if;
        end if;

	case(reg_read_addr_A) is
        	when "100000" =>
            		data_a <= to_unsigned(0, data_a'length);
        	when "100001" =>
            		data_a <= offset_register;
		    when "UUUUUU" =>
			--for first commands
        	when others =>
            		data_a <= reg_array(to_integer(unsigned(reg_read_addr_A(adr_Width-2 downto 0)))); --only 5 bit for allpurpose registers
    	end case;

    	case(reg_read_addr_B) is
        	when "100000" =>
            		data_b <= to_unsigned(0, data_b'length);
        	when "100001" =>
            		data_b <= offset_register;
        	when "UUUUUU" => 
			--for first commands
        	when others =>
            		data_b <= reg_array(to_integer(unsigned(reg_read_addr_B(adr_Width-2 downto 0)))); --only 5 bit for allpurpose registers
    	end case;
    end process;
    
    reg_read_data_A <= data_a;
    reg_read_data_B <= data_b;

end behavior;
