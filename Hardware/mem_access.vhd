--Memory Access Unit

--entity mem_access

--architecture

--INCOMPLETE, NOT IMPLEMENTED

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use STD.textio.all;
use ieee.std_logic_textio.all;
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
--type mem_data is array (0 to 255) of unsigned(bit_Width-1 downto 0); --256 for testing, larger in actual project

subtype word_t  is unsigned(bit_Width- 1 downto 0);
type    ram_t   is array(0 to 256- 1) of word_t; --temporary depth
impure function mem_read_file(FileName : STRING) return ram_t is --function to read file into RAM
file FileHandle       : TEXT open READ_MODE is FileName;
variable CurrentLine  : LINE;
variable TempWord     : std_logic_vector(bit_Width- 1 downto 0); -- was (div_ceil(word_t'length, 4) * 4) - 1 downto 0
variable Result       : ram_t    := (others => (others => '0'));

begin
  for i in 0 to 256- 1 loop --temporary depth
    exit when endfile(FileHandle);

    readline(FileHandle, CurrentLine);
    hread(CurrentLine, TempWord);
    Result(i)    := resize(unsigned(TempWord), word_t'length);
  end loop;

  return Result;
end function;


signal ram : ram_t := mem_read_file("memory.txt");
signal mem_read_data : unsigned(bit_Width-1 downto 0); 

begin
    process(clk, rst, mem_read_on, mem_write_on)
    begin
        if(rst = '1') then
            ram <= mem_read_file("memory.txt");
        else
            if(mem_read_on = '1') then
                mem_read_data <= ram(to_integer(mem_address)); 
            else 
                mem_read_data <= to_unsigned(0, mem_read_data'length);
            end if;
            if(mem_write_on = '1') then --maybe on rising edge?
                ram(to_integer(mem_address)) <= mem_write_data;
            end if;
        end if;
    end process;

    mem_out <= mem_read_data;

end behavior;

