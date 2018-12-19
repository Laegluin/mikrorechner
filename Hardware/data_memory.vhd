--Data Memory (RW-Memory)

--entity data_memory

--architecture

--reads from hex file 

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use STD.textio.all;
use ieee.std_logic_textio.all;
use work.universal_constants.all;

entity data_memory is
port
(
    clk, rst: in std_logic;
    mem_address: in unsigned(bit_Width-1 downto 0); --assuming 32Bit address
    mem_write_data: in unsigned(bit_Width-1 downto 0);
    mem_rw_en: in unsigned(1 downto 0);
    mem_out: out unsigned(bit_Width-1 downto 0)
);
end data_memory;

architecture behavior of data_memory is

subtype word_t  is unsigned(bit_Width- 1 downto 0);
type    ram_t   is array(0 to mem_Depth- 1) of word_t;
impure function mem_read_file(FileName : STRING) return ram_t is --function to read file into RAM
file FileHandle       : TEXT open READ_MODE is FileName;
variable CurrentLine  : LINE;
variable TempWord     : std_logic_vector(bit_Width- 1 downto 0); -- was (div_ceil(word_t'length, 4) * 4) - 1 downto 0
variable Result       : ram_t    := (others => (others => '0'));

begin
  for i in 0 to mem_Depth- 1 loop 
    exit when endfile(FileHandle);

    readline(FileHandle, CurrentLine);
    hread(CurrentLine, TempWord);
    Result(i)    := resize(unsigned(TempWord), word_t'length);
  end loop;

  return Result;
end function;


signal ram : ram_t := mem_read_file("/informatik2/students/home/6lahann/Projekt/work/data_mem.hex");
		--full filepath must always be specified!
signal mem_read_data : unsigned(bit_Width-1 downto 0); 

begin
    process(clk, rst)
    begin
        if(rst = '1') then
            ram <= mem_read_file("/informatik2/students/home/6lahann/Projekt/work/data_mem.hex");
		--full filepath must always be specified!
        else
            if(mem_rw_en = "01") then
                if(rising_edge(clk)) then
                    ram(to_integer(mem_address(2 downto 0))) <= mem_write_data;
                end if;
            elsif(mem_rw_en = "10") then
                mem_read_data <= ram(to_integer(mem_address(2 downto 0))); 
            else
                mem_read_data <= to_unsigned(0, mem_read_data'length);
            end if;
                
        end if;
    end process;

    mem_out <= mem_read_data;

end behavior;


