--Instruction Memory (Read-Only Memory)

--entity instruction_memory

--architecture

--reads from hex file 

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use STD.textio.all;
use ieee.std_logic_textio.all;
use work.universal_constants.all;

entity instruction_memory is
port
(
    clk, rst: in std_logic;
    mem_address: in unsigned(bit_Width-1 downto 0); --assuming 32Bit address
    mem_out: out unsigned(bit_Width-1 downto 0)
);
end instruction_memory;

architecture behavior of instruction_memory is

subtype word_t  is unsigned(7 downto 0);
type    rom_t   is array(0 to (83*4)- 1) of word_t; -- was mem_Depth, just testing
impure function mem_read_file(FileName : STRING) return rom_t is --function to read file into rom
file FileHandle       : TEXT open READ_MODE is FileName;
variable CurrentLine  : LINE;
variable TempWord     : std_logic_vector(bit_Width- 1 downto 0); -- was (div_ceil(word_t'length, 4) * 4) - 1 downto 0
variable Result       : rom_t    := (others => (others => '0'));

begin
  for i in 0 to 83- 1 loop -- was mem_Depth, just testing
    exit when endfile(FileHandle);

    readline(FileHandle, CurrentLine);
    hread(CurrentLine, TempWord);
    Result((4*i)+3)    := resize(unsigned(TempWord(31 downto 24)), word_t'length);
    Result((4*i)+2)  := resize(unsigned(TempWord(23 downto 16)), word_t'length);
    Result((4*i)+1)  := resize(unsigned(TempWord(15 downto 8)), word_t'length);
    Result((4*i)+0)  := resize(unsigned(TempWord(7 downto 0)), word_t'length);
  end loop;

  return Result;
end function;


signal rom : rom_t := mem_read_file("/informatik2/students/home/6lahann/Projekt/work/instruction_mem.hex");
		--full filepath must always be specified!
signal mem_read_data : unsigned(bit_Width-1 downto 0); 

begin
    process(clk, rst)
    begin
        if(rst = '1') then
            rom <= mem_read_file("/informatik2/students/home/6lahann/Projekt/work/instruction_mem.hex");
		--full filepath must always be specified!
        else
            if(rising_edge(clk)) then
                mem_read_data(31 downto 24) <= rom(to_integer(mem_address)+3);
                mem_read_data(23 downto 16) <= rom(to_integer(mem_address)+2);
                mem_read_data(15 downto 8) <= rom(to_integer(mem_address)+1);
                mem_read_data(7 downto 0) <= rom(to_integer(mem_address)+0);
            end if;        
        end if;
    end process;

    mem_out <= mem_read_data;

end behavior;


