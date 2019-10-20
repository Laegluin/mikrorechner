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
	end component;

    signal clk, rst: std_logic;
	signal mem_address : unsigned(bit_Width-1 downto 0);
	signal mem_offset : unsigned(bit_Width-1 downto 0);
	signal mem_rw_en : unsigned(1 downto 0);
	signal C_in : unsigned(bit_Width-1 downto 0);

		-- inputs for passing on
	signal pc_write_enable_in  : std_logic;
	signal reg_imm_in          : unsigned(bit_Width-1 downto 0);
	signal wb_control_in       : unsigned(1 downto 0);
	signal C_address_in        : unsigned(adr_Width-1 downto 0);
	signal jump_in             : unsigned(bit_Width-1 downto 0);

		--outputs from processing
	signal mem_out             : unsigned(bit_Width-1 downto 0);
		
		-- outputs from passing on
	signal pc_write_enable_out : std_logic;
	signal reg_imm_out         : unsigned(bit_Width-1 downto 0);
	signal wb_control_out      : unsigned(1 downto 0);
	signal C_address_out       : unsigned(adr_Width-1 downto 0);
	signal jump_out            : unsigned(bit_Width-1 downto 0);
	signal C_out               : unsigned(bit_Width-1 downto 0);

    signal dump : std_logic;


    begin
	uut: data_memory port map
	(
	    clk => clk,
		rst => rst,

		-- inputs for processing
		mem_address => mem_address,
		mem_offset => mem_offset,
		mem_rw_en => mem_rw_en,
		C_in => C_in,

		-- inputs for passing on
		pc_write_enable_in => pc_write_enable_in,
		reg_imm_in => reg_imm_in,
		wb_control_in => wb_control_in,
		C_address_in => C_address_in,
		jump_in => jump_in,

		--outputs from processing
		mem_out => mem_out,
		
		-- outputs from passing on
		pc_write_enable_out => pc_write_enable_out,
		reg_imm_out => reg_imm_out,
		wb_control_out => wb_control_out,
		C_address_out => C_address_out,
		jump_out => jump_out,
		C_out => C_out,

		-- testing
		dump => dump
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
	    dump <= '0';
		wait for 100 ns;
		dump <= '1';
		--wait for 100 ns;
		--dump <= '0';
		--rst <= '1';
		--wait for 100 ns;
		--rst <= '0';
		wait;
	end process;
end test_mem;
 

