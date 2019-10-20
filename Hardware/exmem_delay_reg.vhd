--Delay Register

--entity exmem_delay_reg

--architecture 

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity exmem_delay_reg is
port
(
    clk, rst: in std_logic;
    PC_write_enable_in : in std_logic;
    wb_control_in : in unsigned(1 downto 0);
    jump_to_in : in unsigned(bit_Width-1 downto 0);
    reg_imm_in : in unsigned(bit_Width-1 downto 0);
    mem_off_in : in unsigned(bit_Width-1 downto 0);
    mem_rw_en_in : in unsigned(1 downto 0);
    C_addr_in : in unsigned(adr_Width-1 downto 0);

    PC_write_enable_out : out std_logic;
    wb_control_out : out unsigned(1 downto 0);
    jump_to_out : out unsigned(bit_Width-1 downto 0);
    reg_imm_out : out unsigned(bit_Width-1 downto 0);
    mem_off_out : out unsigned(bit_Width-1 downto 0);
    mem_rw_en_out : out unsigned(1 downto 0);
    C_addr_out : out unsigned(adr_Width-1 downto 0)
);
end exmem_delay_reg;

architecture behavior of exmem_delay_reg is

signal tmp_PC_write_enable : std_logic := '0';
signal tmp_wb_control : unsigned(1 downto 0) := (others => '0');
signal tmp_jump_to: unsigned(bit_Width-1 downto 0) := (others => '0');
signal tmp_reg_imm: unsigned(bit_Width-1 downto 0) := (others => '0');
signal tmp_mem_off: unsigned(bit_Width-1 downto 0) := (others => '0');
signal tmp_mem_rw_en : unsigned(1 downto 0) := (others => '0');
signal tmp_C_addr : unsigned(adr_Width-1 downto 0) := (others => '0');

begin
    process(clk, rst)
    begin
        if(rst='1') then

            tmp_PC_write_enable <= '0';
            tmp_wb_control <= (others => '0');
            tmp_jump_to <= (others => '0');
            tmp_reg_imm <= (others => '0');
            tmp_mem_off <= (others => '0');
            tmp_mem_rw_en <= (others => '0');
            tmp_C_addr <= (others => '0');

        else
			if rising_edge(clk) then

				tmp_PC_write_enable <= PC_write_enable_in;
				tmp_wb_control <= wb_control_in;
                tmp_jump_to <= jump_to_in;
                tmp_reg_imm <= reg_imm_in;
                tmp_mem_off <= mem_off_in;
                tmp_mem_rw_en <= mem_rw_en_in;
                tmp_C_addr <= C_addr_in;

			end if;
        end if;

    end process;

    PC_write_enable_out <= tmp_PC_write_enable;
    wb_control_out <= tmp_wb_control;
    jump_to_out <= tmp_jump_to;
    reg_imm_out <= tmp_reg_imm;
    mem_off_out <= tmp_mem_off;
    mem_rw_en_out <= tmp_mem_rw_en;
    C_addr_out <= tmp_C_addr;

end behavior;
