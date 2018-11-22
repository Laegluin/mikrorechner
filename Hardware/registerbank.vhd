-- 32-Bit-Register File

--entity registerbank

--architecture

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity register_file is
generic(
    bit_width: integer := 32;
    address_width : integer := 6 --or 5 if special regs in file
);
port(
    clk, rst: in std_logic;
    reg_write_on: in std_logic;
    reg_write_addr: in std_logic_vector(address_width-1 downto 0);
    reg_write_data: in std_logic_vector(bit_width-1 downto 0);
    reg_read_addr_A: in std_logic_vector(5 downto 0);
    reg_read_data_A: out std_logic_vector(bit_width-1 downto 0);
    reg_read_addr_B: in std_logic_vector(5 downto 0);
    reg_read_data_B: out std_logic_vector(bit_width-1 downto 0)
);
end register_file;

architecture behavior of register_file is
type reg_type is array (0 to bit_width-1) of std_logic_vector(bit_width-1 downto 0);
signal reg_array : reg_type;
signal data_a : std_logic_vector(bit_width-1 downto 0);
signal data_b : std_logic_vector(bit_width-1 downto 0);

begin
    process(clk, rst, reg_read_addr_A, reg_read_addr_B)
	variable offset_register : std_logic_vector(bit_width-1 downto 0) := std_logic_vector(to_signed(0, bit_width)); --not sure if this is smart...
    begin
        if(rst='1') then
            --maybe do something
        elsif(rising_edge(clk)) then
            if(reg_write_on='1') then
                if(reg_write_addr="100001") then
                    offset_register := reg_write_data;
                else    
                    reg_array(to_integer(unsigned(reg_write_addr(address_width-2 downto 0)))) <= reg_write_data;
                end if;
	    end if;
        end if;

	case(reg_read_addr_A) is
        	when "100000" =>
            		data_a <= std_logic_vector(to_unsigned(0, data_a'length));
        	when "100001" =>
            		data_a <= offset_register;
        	when "------" => --don't care or U uninitialized?
            		--do something here maybe?
        	when others =>
            		data_a <= reg_array(to_integer(unsigned(reg_read_addr_A(address_width-2 downto 0)))); --only 5 bit for allpurpose registers
    	end case;

    	case(reg_read_addr_B) is
        	when "100000" =>
            		data_b <= std_logic_vector(to_unsigned(0, data_b'length));
        	when "100001" =>
            		data_b <= offset_register;
        	when "------" => --don't care or U uninitialized?
            		--do something here maybe?
        	when others =>
            		data_b <= reg_array(to_integer(unsigned(reg_read_addr_B(address_width-2 downto 0)))); --only 5 bit for allpurpose registers
    	end case;
    end process;
    
    reg_read_data_A <= data_a;
    reg_read_data_B <= data_b;

end behavior;