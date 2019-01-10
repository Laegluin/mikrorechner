-- testbench for Execute-Stage of 32-Bit-CPU

-- entity test_executer

-- architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity test_executer is

end test_executer;

architecture behavior of test_executer is
    -- Unit Under Test (UUT)
    component executer

    port
    (
        clk, enable   : in       std_logic;
        reset         : in       std_logic;
        pc_in         : in       unsigned(bit_Width-1 downto 0);
        jump_off_in   : in       unsigned(bit_Width-1 downto 0);
        mem_off_in    : in       unsigned(bit_Width-1 downto 0);
        reg_imm_in    : in       unsigned(bit_Width-1 downto 0);
        opcode_in     : in       unsigned(opcode_Bits-1 downto 0);
        alu_flag      : in       std_logic;
        C_in          : in       unsigned(adr_Width-1 downto 0); -- register adress of alu-result

        C_out         : out      unsigned(adr_Width-1 downto 0);
        pc_enable     : out      std_logic;
        pc_write_en   : out      std_logic;
        mem_rw_en     : out      unsigned(1 downto 0);
        reg_imm_out   : out      unsigned(bit_Width-1 downto 0);
        wb_control    : out      unsigned(1 downto 0);
        jump_to_out   : out      unsigned(bit_Width-1 downto 0);
        mem_off_out   : out      unsigned(bit_Width-1 downto 0);
        opcode_out    : out      unsigned(opcode_Bits-1 downto 0)
);

    end component executer;

    -- inputs
    signal clk, enable  : std_logic;
    signal reset        : std_logic;
    signal pc_in        : unsigned(bit_Width-1 downto 0);
    signal jump_off_in  : unsigned(bit_Width-1 downto 0);
    signal mem_off_in   : unsigned(bit_Width-1 downto 0);
    signal reg_imm_in   : unsigned(bit_Width-1 downto 0);
    signal opcode_in    : unsigned(opcode_Bits-1 downto 0);
    signal alu_flag     : std_logic;
    signal C_in         : unsigned(adr_Width-1 downto 0);

    -- outputs
    signal C_out        : unsigned(adr_Width-1 downto 0);
    signal pc_enable    : std_logic;
    signal pc_write_en  : std_logic;
    signal mem_rw_en    : unsigned(1 downto 0);
    signal reg_imm_out  : unsigned(bit_Width-1 downto 0);
    signal wb_control   : unsigned(1 downto 0);
    signal jump_to_out  : unsigned(bit_Width-1 downto 0);
    signal mem_off_out  : unsigned(bit_Width-1 downto 0);
    signal opcode_out   : unsigned(opcode_Bits-1 downto 0);



begin
    -- instanziiere UUT
    uut: executer port map
    (
        clk         => clk,
        enable      => enable,
        reset       => reset,

        pc_in       => pc_in,
        jump_off_in => jump_off_in,
        mem_off_in  => mem_off_in,

        reg_imm_in  => reg_imm_in,
        opcode_in   => opcode_in,
        alu_flag    => alu_flag,

        C_in        => C_in,
        C_out       => C_out,

        pc_enable   => pc_enable,
        pc_write_en => pc_write_en,
        mem_rw_en   => mem_rw_en,
        reg_imm_out => reg_imm_out,
        wb_control  => wb_control,

        jump_to_out => jump_to_out,
        mem_off_out => mem_off_out,
        opcode_out  => opcode_out
    );

    -- clk process
    clk_proc: process
    begin
        clk <= '0';
        wait for 10 ns;
        clk <= '1';
        wait for 10 ns;
    end process;

    -- Stimulus process
    stim_proc: process
    begin

    -- reset
    reset <= '1';
    wait for 100 ns;
    reset <= '0';
    
    -- Testwerte
    C_in       <= (others => '0');
    mem_off_in <= (others => '0');
    reg_imm_in <= (others => '0');

    enable <= '0';
    wait for 100 ns;

    jump_off_in <= "00000000111111110000000011110000";

    pc_in <=       "00000000000000000000000000000000";

    alu_flag <= '1';

    enable <= '1';

        -- JMP
    opcode_in <= "00110";
    wait for 100 ns;

        -- JMP REL
    opcode_in <= "00111";
    wait for 100 ns;

    -- JMP IF
    opcode_in <= "01000";
    wait for 100 ns;

    -- JMP REL IF
    opcode_in <= "01001";
    wait for 100 ns;

        -- alu_flag = 0

    alu_flag <= '0';

    -- JMP IF
    opcode_in <= "01000";
    wait for 100 ns;

    -- JMP REL IF
    opcode_in <= "01001";
    wait for 100 ns;

        -- test reset
    enable <= '0';
    wait for 100 ns;



    assert False report "Ende des Tests erreicht.";
    wait;

    end process;


end architecture behavior;

