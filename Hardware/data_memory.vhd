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
    mem_offset: in unsigned(bit_Width-1 downto 0);
    mem_write_data: in unsigned(bit_Width-1 downto 0);
    mem_rw_en: in unsigned(1 downto 0);
    mem_out: out unsigned(bit_Width-1 downto 0);

    pc_enable_in : in std_logic;
    pc_write_enable_in : in std_logic;
    C_in : in unsigned(bit_Width-1 downto 0);
    wb_control_in : in unsigned(1 downto 0);
    reg_imm_in : in unsigned(bit_Width-1 downto 0);
    c_address_in : in unsigned(adr_Width-1 downto 0);

    c_address_out : out unsigned(adr_Width-1 downto 0);
    jump_out : out unsigned(bit_Width-1 downto 0);
    reg_imm_out : out unsigned(bit_Width-1 downto 0);
    wb_control_out : out unsigned(1 downto 0);
    C_out : out unsigned(bit_Width-1 downto 0);
    pc_write_enable_out : out std_logic;
    pc_enable_out : out std_logic;
);
end data_memory;

architecture behavior of data_memory is

subtype word_t  is unsigned(7 downto 0);
type    ram_t   is array(0 to (mem_Depth*4)- 1) of word_t;
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
    Result(4*i)    := resize(unsigned(TempWord(31 downto 24)), word_t'length);
    Result((4*i)+1)  := resize(unsigned(TempWord(23 downto 16)), word_t'length);
    Result((4*i)+2)  := resize(unsigned(TempWord(15 downto 8)), word_t'length);
    Result((4*i)+3)  := resize(unsigned(TempWord(7 downto 0)), word_t'length);
  end loop;

  return Result;
end function;


signal ram : ram_t := mem_read_file("/informatik2/students/home/6lahann/Projekt/work/data_mem.hex");
		--full filepath must always be specified!
signal mem_read_data : unsigned(bit_Width-1 downto 0);
signal address : unsigned(bit_Width-1 downto 0); 

begin
    process(clk, rst)
    begin
        address <= mem_address + mem_offset;
        if(rst = '1') then
            ram <= mem_read_file("/informatik2/students/home/6lahann/Projekt/work/data_mem.hex");
		--full filepath must always be specified!
        else
            if(mem_rw_en = "01") then
                if(rising_edge(clk)) then
                    ram(to_integer(address)) <= mem_write_data(31 downto 24);
                    ram(to_integer(address)+1) <= mem_write_data(23 downto 16);
                    ram(to_integer(address)+2) <= mem_write_data(15 downto 8);
                    ram(to_integer(address)+3) <= mem_write_data(7 downto 0);
                    
                    --ram(to_integer(mem_address(2 downto 0))) <= mem_write_data;
                end if;
            elsif(mem_rw_en = "10") then
                mem_read_data(31 downto 24) <= ram(to_integer(address));
                mem_read_data(23 downto 16) <= ram(to_integer(address)+1);
                mem_read_data(15 downto 8) <= ram(to_integer(address)+2);
                mem_read_data(7 downto 0) <= ram(to_integer(address)+3); 
            else
                mem_read_data <= to_unsigned(0, mem_read_data'length);
            end if;
                
        end if;
    end process;

    mem_out <= mem_read_data;

    pc_enable_out <= pc_enable_in;
    pc_write_enable_out <= pc_write_enable_in;
    wb_control_out <= wb_control_in;
    reg_imm_out <= reg_imm_in;
    C_out <= C_in;
    jump_out <= C_in when pc_write_enable_in = '1';
    c_address_out <= c_address_in;

end behavior;


