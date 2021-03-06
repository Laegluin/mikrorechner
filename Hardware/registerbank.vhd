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
    reg_write_en: in std_logic;
    reg_offset_en: in std_logic;
    reg_write_addr: in unsigned(adr_Width-1 downto 0);
    reg_write_data: in unsigned(bit_Width-1 downto 0);
    reg_read_addr_A: in unsigned(adr_Width-1 downto 0);
    reg_read_data_A: out unsigned(bit_Width-1 downto 0);
    reg_read_addr_B: in unsigned(adr_Width-1 downto 0);
    reg_read_data_B: out unsigned(bit_Width-1 downto 0);
    reg_read_addr_C: in unsigned(adr_Width-1 downto 0); --only for STORE
    reg_read_data_C: out unsigned(bit_Width-1 downto 0) --only for STORE
);
end registerbank;

architecture behavior of registerbank is
type reg_type is array (0 to (2**(adr_Width-1))-1) of unsigned(bit_Width-1 downto 0);
signal reg_array : reg_type := (others => (others => '0'));
signal data_a : unsigned(bit_Width-1 downto 0);
signal data_b : unsigned(bit_Width-1 downto 0);
signal data_c : unsigned(bit_Width-1 downto 0);

begin
    process(clk, rst)
	variable offset_register : unsigned(bit_Width-1 downto 0) := (others => '0'); 
    begin
        if rst='1' then

            reg_array <= (others => (others => '0'));

        elsif rising_edge(clk) then
            if reg_write_en='1' then
                if reg_write_addr="100001" then
                    offset_register := reg_write_data;
                else    
                    reg_array(to_integer(unsigned(reg_write_addr(adr_Width-2 downto 0)))) <= reg_write_data;
                end if;
	        end if;
        end if;

	    case(reg_read_addr_A) is
        	when "100000" =>
            		data_a <= (others => '0');
        	when "100001" =>
            		data_a <= offset_register;
        	when others =>
            		data_a <= reg_array(to_integer(unsigned(reg_read_addr_A(adr_Width-2 downto 0)))); --only 5 bit for allpurpose registers
    	end case;

        if reg_offset_en='1' then
      
            data_b <= offset_register;
        
        else
        
            case(reg_read_addr_B) is
                when "100000" =>
                        data_b <= (others => '0');
                when "100001" =>
                        data_b <= offset_register;
                when others =>
                        data_b <= reg_array(to_integer(unsigned(reg_read_addr_B(adr_Width-2 downto 0)))); --only 5 bit for allpurpose registers
            end case;
        
        end if;

        case(reg_read_addr_C) is
        	when "100000" =>
            		data_c <= (others => '0');
        	when "100001" =>
            		data_c <= offset_register;
        	when others =>
            		data_c <= reg_array(to_integer(unsigned(reg_read_addr_C(adr_Width-2 downto 0)))); --only 5 bit for allpurpose registers
    	end case;

    end process;
    
    reg_read_data_A <= data_a;
    reg_read_data_B <= data_b;
    reg_read_data_C <= data_c;

end behavior;
