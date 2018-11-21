-- 32-Bit-Register File

--entity registerbank

--architecture

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity register_file is
generic(
    bit_width: integer := 32;
    address_width : integer := 6; --or 5 if special regs in file
);
port(
    clk, rst: in std_logic;
    reg_write_on: in std_logic;
    reg_write_addr: in std_logic_vector(address_width-1 downto 0);
    reg_write_data: in std_logic_vector(bit_width-1 downto 0);
    reg_read_addr_A: in std_logic_vector(address_width-1 downto 0);
    reg_read_data_A: out std_logic_vector(bit_width-1 downto 0);
    reg_read_addr_B: in std_logic_vector(address_width-1 downto 0);
    reg_read_data_B: out std_logic_vector(bit_width-1 downto 0);
);
end register_file;

architecture behavior of register_file is
type reg_type is array (0 to bit_width-1) of std_logic_vector(bit_width-1 downto 0);
signal reg_array : reg_type;
variable offset_register : std_logic_vector(bit_width-1 downto 0) := std_logic_vector(to_unsigned(0, offset_register'length));
begin
    process(clk, rst)
        begin
        if(rst='1') then
            --start code here?
        elsif(rising_edge(clk)) then
            if(reg_write_on='1') then
                if(reg_write_addr='100001') then
                    offset_register := reg_write_data;
                else    
                    reg_array(to_integer(unsigned(reg_write_addr))) <= reg_write_data;
                end if;
            end if;
        end if;
    end process;
    
    case(reg_read_addr_A) is
        when '100000' =>
            reg_read_data_A <= std_logic_vector(to_unsigned('0', reg_read_data_A'length));
        when '100001' =>
            reg_read_data_A <= offset_register;
        when "------" => --don't care or U uninitialized?
            --do something here maybe?
        others =>
            reg_read_data_A <= reg_array(to_integer(unsigned(reg_read_addr_A-2 downto 0))); --only 5 bit for allpurpose registers
    end case;

    case(reg_read_addr_B) is
        when '100000' =>
            reg_read_data_B <= std_logic_vector(to_unsigned('0', reg_read_data_B'length));
        when '100001' =>
            reg_read_data_B <= offset_register;
        when "------" => --don't care or U uninitialized?
            --do something here maybe?
        others =>
            reg_read_data_B <= reg_array(to_integer(unsigned(reg_read_addr_B-2 downto 0))); --only 5 bit for all-purpose-registers
    end case;

end behavior;