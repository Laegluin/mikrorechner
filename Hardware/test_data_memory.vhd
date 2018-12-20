library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity test_memory is
end test_memory;

architecture test_mem of test_memory is
	
	component data_memory
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
    	jump_in  : in unsigned(bit_Width-1 downto 0);
    	jump_out : out unsigned(bit_Width-1 downto 0);
    	reg_imm_out : out unsigned(bit_Width-1 downto 0);
    	wb_control_out : out unsigned(1 downto 0);
    	C_out : out unsigned(bit_Width-1 downto 0);
    	pc_write_enable_out : out std_logic;
    	pc_enable_out : out std_logic
	);
	end component;

    signal clk, rst: std_logic;
    signal mem_address: unsigned(bit_Width-1 downto 0);
	signal mem_offset: unsigned(bit_Width-1 downto 0);
	signal mem_rw_en: unsigned(1 downto 0);
    signal mem_write_data: unsigned(bit_Width-1 downto 0);
    signal mem_out: unsigned(bit_Width-1 downto 0);
	
	signal pc_enable_in, pc_write_enable_in, pc_enable_out, pc_write_enable_out: std_logic;
	signal wb_control_in, wb_control_out: unsigned(1 downto 0);
	signal C_in, C_out, reg_imm_in, reg_imm_out, jump_in, jump_out: unsigned(bit_Width-1 downto 0);
	signal c_address_in, c_address_out: unsigned(adr_Width-1 downto 0);


    begin
	uut: data_memory port map
	(
	    clk => clk,
	    rst => rst,
	    mem_address => mem_address,
    	mem_offset => mem_offset,
    	mem_write_data => mem_write_data,
    	mem_rw_en => mem_rw_en,
    	mem_out => mem_out,

    	pc_enable_in => pc_enable_in,
    	pc_write_enable_in => pc_write_enable_in,
    	C_in => C_in,
    	wb_control_in => wb_control_in,
    	reg_imm_in => reg_imm_in,
    	c_address_in => c_address_in,

    	c_address_out => c_address_out,
    	jump_in  => jump_in,
    	jump_out => jump_out,
    	reg_imm_out => reg_imm_out,
    	wb_control_out => wb_control_out,
    	C_out => C_out,
    	pc_write_enable_out => pc_write_enable_out,
    	pc_enable_out => pc_enable_out
	);

	process
	begin
            clk <= '0';
            wait for 10 ns;
            clk <= '1';
            wait for 10 ns;
        end process;

	process
	begin
	    mem_rw_en <= "10";
		mem_offset <= to_unsigned(0, bit_Width);
	    mem_address <= to_unsigned(0, bit_Width);
	    wait for 40 ns;

	    mem_address <= to_unsigned(4, bit_Width);
	    wait for 40 ns;

	    mem_address <= to_unsigned(8, bit_Width);
	    wait for 40 ns;

	    mem_address <= to_unsigned(12, bit_Width);
	    wait for 40 ns;

	    mem_address <= to_unsigned(16, bit_Width);
	    wait for 40 ns;

	    mem_address <= to_unsigned(20, bit_Width);
	    wait for 40 ns;

	    mem_address <= to_unsigned(24, bit_Width);
	    wait for 40 ns;

	    mem_address <= to_unsigned(28, bit_Width);
	    wait for 40 ns;

	    mem_rw_en <= "00";
	    wait for 40 ns;

	    mem_address <= to_unsigned(16, bit_Width);
	    mem_write_data <= to_unsigned(9, bit_Width);
	    mem_rw_en <= "01";
	    wait for 20 ns;

	    mem_rw_en <= "10";
	    wait for 40 ns;

	    mem_address <= to_unsigned(20, bit_Width);
	    wait for 40 ns;

	    mem_address <= to_unsigned(24, bit_Width);
	    wait for 40 ns;

	    rst <= '1';
	    wait for 100 ns;
	    rst <= '0';
	    wait for 100 ns;
	end process;
end test_mem;
 

