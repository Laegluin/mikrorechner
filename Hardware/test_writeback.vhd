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
    	write_back_data: out unsigned(bit_Width-1 downto 0)
    );
    end component;
     
    component mux32
    port
    (
		SEL: in std_logic;
		A: in unsigned(bit_Width-1 downto 0);
		B: in unsigned(bit_Width-1 downto 0);
		X: out unsigned(bit_Width-1 downto 0)
    );
    end component;

    component registerbank
    port
    (
		clk, rst: in std_logic;
		reg_write_en: in std_logic;
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
    signal reg_write_en, reg_sel: std_logic;
    signal reg_write_data, write_back_data: unsigned(bit_Width-1 downto 0);
	signal reg_write_FINAL: unsigned(bit_Width-1 downto 0);
	
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
            write_back_data => write_back_data
        );
		
		uut_reg: registerbank port map
		(
			clk => clk,
            rst => rst,
            reg_write_en => reg_write_en,
            reg_write_addr => reg_write_addr,
            reg_write_data => reg_write_FINAL,
            reg_read_addr_A => reg_read_addr_A,
            reg_read_data_A => reg_read_data_A,
            reg_read_addr_B => reg_read_addr_B,
            reg_read_data_B => reg_read_data_B
		);

		uut_mux: mux32 port map
		(
			SEL => reg_sel,
			A => reg_write_data,
			B => write_back_data,
			X => reg_write_FINAL
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
			--wait for 5 ns;
			--sel : 1 writes from IMM, 0 from Write-Back
			reg_write_en <= '1';
			reg_write_addr <= to_unsigned(6, adr_Width);
			reg_write_data <= to_unsigned(1023, bit_Width);
			ALU_Out <= to_unsigned(4, bit_Width);
			mem_out <= to_unsigned(2, bit_Width);
			reg_sel <= '1';
			wait for 20 ns; 
			-- should write 1023 to register 6
			reg_write_en <= '0';
			
			wait for 100 ns;

			reg_write_addr <= to_unsigned(4, adr_Width);
			reg_read_addr_B <= to_unsigned(4, adr_Width);
			
			wb_control <= "10"; --ALU
			reg_read_addr_A <= to_unsigned(6, adr_Width);
			reg_sel <= '0';
			reg_write_en <= '1';
			wait for 20 ns;
			--should write 4 to register 4, read B should be 4, read A should be 1023
			reg_write_en <= '0';
			wait for 100 ns;
			
			reg_write_addr <= to_unsigned(2, adr_Width);
			wb_control <= "11"; --REG
			reg_sel <= '0'; --not needed but for consistency
			reg_write_en <= '1';
			wait for 20 ns;
			--should write 1023 to register 2
			reg_write_en <= '0';  

			reg_read_addr_B <= to_unsigned(2, adr_Width);
			--read B should be 1023, A should be 1023			
			wait for 1000 ns;
        end process;
end test_wb;
