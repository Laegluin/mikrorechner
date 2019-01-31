--Data Memory (RW-Memory)

--entity data_memory

--architecture

--reads from hex file 

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;
use work.universal_constants.all;

entity data_memory is
port
(
    -- control signals
    clk, rst            : in std_logic;

    -- inputs for processing
    mem_address         : in unsigned(bit_Width-1 downto 0);
    mem_offset          : in unsigned(bit_Width-1 downto 0);
    mem_rw_en           : in unsigned(1 downto 0);
    C_in                : in unsigned(bit_Width-1 downto 0);

    -- inputs for passing on
    pc_write_enable_in  : in std_logic;
    reg_imm_in          : in unsigned(bit_Width-1 downto 0);
    wb_control_in       : in unsigned(1 downto 0);
    C_address_in        : in unsigned(adr_Width-1 downto 0);
    jump_in             : in unsigned(bit_Width-1 downto 0);

    --outputs from processing
    mem_out             : out unsigned(bit_Width-1 downto 0);
    
    -- outputs from passing on
    pc_write_enable_out : out std_logic;
    reg_imm_out         : out unsigned(bit_Width-1 downto 0);
    wb_control_out      : out unsigned(1 downto 0);
    C_address_out       : out unsigned(adr_Width-1 downto 0);
    jump_out            : out unsigned(bit_Width-1 downto 0);
    C_out               : out unsigned(bit_Width-1 downto 0);

    -- testing
    dump : in std_logic
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
    read(CurrentLine, TempWord);
    Result((4*i)+3)  := resize(unsigned(TempWord(31 downto 24)), word_t'length);
    Result((4*i)+2)  := resize(unsigned(TempWord(23 downto 16)), word_t'length);
    Result((4*i)+1)  := resize(unsigned(TempWord(15 downto 8)), word_t'length);
    Result((4*i)+0)  := resize(unsigned(TempWord(7 downto 0)), word_t'length);
  end loop;

  return Result;
end function;

--signal ram : ram_t := mem_read_file("/informatik2/students/home/6lahann/Projekt/work/data_mem.hex");
signal ram : ram_t := mem_read_file("C:/Users/Moritz Lahann/Desktop/STUDIUM/PROJEKT MIKROPROZESSOR/GIT/Hardware/data_mem.dat");
		--full filepath must always be specified!
signal mem_read_data : unsigned(bit_Width-1 downto 0);
signal mem_write_data : unsigned(bit_Width-1 downto 0);
signal address : unsigned(bit_Width-1 downto 0); 

begin
    process(clk, rst)
    begin

        mem_write_data <= C_in;

        if rst = '1' then

            --ram <= mem_read_file("/informatik2/students/home/6lahann/Projekt/work/data_mem.hex");
            ram <= mem_read_file("C:/Users/Moritz Lahann/Desktop/STUDIUM/PROJEKT MIKROPROZESSOR/GIT/Hardware/data_mem.dat");
		--full filepath must always be specified!
        elsif rising_edge(clk) then

            if mem_rw_en = "01" then --STORE

                address <= mem_address + mem_offset;

                if address > "01111111111111111111111111111111" then --Area for Data Memory is 0x80000000 upwards (including that), remove for simulation?

                    ram(to_integer(address))    <= mem_write_data(31 downto 24);
                    ram(to_integer(address)+1)  <= mem_write_data(23 downto 16);
                    ram(to_integer(address)+2)  <= mem_write_data(15 downto 8);
                    ram(to_integer(address)+3)  <= mem_write_data(7 downto 0);
                
                else

                    mem_read_data <= (others => '0');

                end if;
                    
            elsif mem_rw_en = "10" then --LOAD

                address <= C_in + mem_offset;

                if address > "01111111111111111111111111111111" then --Area for Data Memory is 0x80000000 upwards (including that), remove for simulation?

                    mem_read_data(31 downto 24) <= ram(to_integer(address));
                    mem_read_data(23 downto 16) <= ram(to_integer(address)+1);
                    mem_read_data(15 downto 8)  <= ram(to_integer(address)+2);
                    mem_read_data(7 downto 0)   <= ram(to_integer(address)+3); 

                else

                    mem_read_data <= (others => '0');

                end if;

            else

                mem_read_data <= (others => '0');

            end if;
                
        end if;
    end process;

    process(dump)

        file data_file : text;
        variable file_line : line;
        variable open_status : file_open_status;

    begin
        if dump = '1' then 
            file_open(open_status, data_file, "C:/Users/Moritz Lahann/Desktop/STUDIUM/PROJEKT MIKROPROZESSOR/GIT/Hardware/mem_dump.dat", write_mode);
            assert open_status = open_ok;
                report "Error opening dump file"
                severity error;
            for i in 0 to mem_Depth - 1 loop
                write(file_line, std_logic_vector(ram(4*i+0)));
                write(file_line, std_logic_vector(ram(4*i+1)));
                write(file_line, std_logic_vector(ram(4*i+2)));
                write(file_line, std_logic_vector(ram(4*i+3)));
                writeline(data_file, file_line);
            end loop; 
            file_close(data_file);
        end if;
    end process;

    -- memory output
    mem_out <= mem_read_data;

    -- unchanged outputs from passing on values
    pc_write_enable_out <= pc_write_enable_in;
    wb_control_out <= wb_control_in;
    reg_imm_out <= reg_imm_in;
    C_out <= C_in;
    C_address_out <= C_address_in;
    
    -- jump output
    -- einer der beiden Werte ist immer Null
    jump_out <= (C_in + jump_in) when pc_write_enable_in = '1';

end behavior;


