--testbench for register write back

--entity test_writeback

--architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity test_writeback is
end test_writeback;

architecture test_wb of test_writeback is
    component reg_write_back
    port
    (
        clk, rst: in std_logic;
        wb_control: in unsigned(1 downto 0); --mem wb, alu wb, copy or no wb
    	ALU_Out: in unsigned(bit_Width-1 downto 0);
    	mem_out: in unsigned(bit_Width-1 downto 0);
    	reg_read_data_A: in unsigned(bit_Width-1 downto 0);
    	reg_write_on: out std_logic;
    	reg_write_data: out unsigned(bit_Width-1 downto 0)
    );
    end component;

    component registerbank
    port
    (
		clk, rst: in std_logic;
		reg_write_on: in std_logic;
        reg_write_addr: in unsigned(adr_Width-1 downto 0);
        reg_write_data: in unsigned(bit_Width-1 downto 0);
        reg_read_addr_A: in unsigned(adr_Width-1 downto 0);
        reg_read_data_A: out unsigned(bit_Width-1 downto 0);
        reg_read_addr_B: in unsigned(adr_Width-1 downto 0);
        reg_read_data_B: out unsigned(bit_Width-1 downto 0)
	);
	end component;
   
    signal clk, rst: std_logic;
    signal wb_control: unsigned(1 downto 0);
    signal ALU_Out, mem_out, reg_read_data_A: unsigned(bit_Width-1 downto 0);
    signal reg_write_on: std_logic;
    signal reg_write_data: unsigned(bit_Width-1 downto 0);
	
	signal reg_write_addr: unsigned(adr_Width-1 downto 0);
	signal reg_read_addr_A: unsigned(adr_Width-1 downto 0);
	signal reg_read_addr_B: unsigned(adr_Width-1 downto 0);
	signal reg_read_data_B: unsigned(bit_Width-1 downto 0);

    begin 
        uut_wb: reg_write_back port map
        (
            clk => clk,
            rst => rst,
            wb_control => wb_control,
            ALU_Out => ALU_Out,
            mem_out => mem_out,
            reg_read_data_A => reg_read_data_A,
            reg_write_on => reg_write_on,
            reg_write_data => reg_write_data
        );
		
		uut_reg: registerbank port map
		(
			clk => clk,
            rst => rst,
            reg_write_on => reg_write_on,
            reg_write_addr => reg_write_addr,
            reg_write_data => reg_write_data,
            reg_read_addr_A => reg_read_addr_A,
            reg_read_data_A => reg_read_data_A,
            reg_read_addr_B => reg_read_addr_B,
            reg_read_data_B => reg_read_data_B
		);

        --clk_proc: process
        process
	begin
            clk <= '0';
            wait for 10 ns;
            clk <= '1';
            wait for 10 ns;
        end process;

        --test_proc: process
	process
        begin
			ALU_Out <= to_unsigned(4, bit_Width);
			mem_out <= to_unsigned(2, bit_Width);			
			--wb_control <= "00";
			--reg_write_on <= '1';
			reg_write_addr <= "000010";
			--reg_write_data <= to_unsigned(1, bit_Width);
			reg_read_addr_B <= "000011";
			wait for 100 ns; 
			
			--reg_write_on <= '0';
			wait for 100 ns;
			
			wb_control <= "10";
			reg_read_addr_A <= "000010";
			wait for 20 ns;
			wb_control <= "00";
			wait for 100 ns;
			
			reg_write_addr <= "000011";
			wb_control <= "01";
			wait for 20 ns;
			
			wb_control <= "00";
			--reg_write_on <= '0';			
			wait for 100 ns;
        end process;
end test_wb;
