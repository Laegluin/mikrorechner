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
        pc_in         : in       unsigned(bit_Width-1 downto 0);
        jump_off_in   : in       signed(bit_Width-1 downto 0);
        mem_off_in    : in       unsigned(bit_Width-1 downto 0);
        reg_imm       : in       unsigned(bit_Width-1 downto 0);
        opcode_in     : in       unsigned(opcode_Bits-1 downto 0);
        alu_flag      : in       std_logic;
        jump_to_in    : in       unsigned(bit_Width-1 downto 0);
        C_in          : in       unsigned(bit_Width-1 downto 0); -- register adress of alu-result

        C_out         : out       unsigned(bit_Width-1 downto 0);
        pc_write_en   : out      std_logic;
        mem_write_en  : out      std_logic;
        reg_write_en  : out      std_logic;
        jump_to_out   : out      unsigned(bit_Width-1 downto 0);
        mem_off_out   : out      unsigned(bit_Width-1 downto 0);
        opcode_out    : out      unsigned(bit_Width-1 downto 0)
);

    end component executer;
    -- inputs
    signal clk, enable  : std_logic;
    signal pc_in        : unsigned(bit_Width-1 downto 0);
    signal jump_off_in  : signed(bit_Width-1 downto 0);
    signal mem_off_in   : unsigned(bit_Width-1 downto 0);
    signal reg_imm      : unsigned(bit_Width-1 downto 0);
    signal opcode_in    : unsigned(opcode_Bits-1 downto 0);
    signal alu_flag     : std_logic;
    signal jump_to_in   : unsigned(bit_Width-1 downto 0);
    signal C_in         : unsigned(bit_Width-1 downto 0);

    -- outputs
    signal C_out        : unsigned(bit_Width-1 downto 0);
    signal pc_write_en  : std_logic;
    signal mem_write_en : std_logic;
    signal reg_write_en : std_logic;
    signal jump_to_out  : unsigned(bit_Width-1 downto 0);
    signal mem_off_out  : unsigned(bit_Width-1 downto 0);
    signal opcode_out   : unsigned(bit_Width-1 downto 0);



begin
    -- instanziiere UUT
    uut: executer port map
    (
        clk    => clk,
        enable => enable,

        pc_in       => pc_in,
        jump_off_in => jump_off_in,
        mem_off_in  => mem_off_in,

        reg_imm    => reg_imm,
        opcode_in  => opcode_in,
        alu_flag   => alu_flag,
        jump_to_in => jump_to_in,

        C_in  => C_in,
        C_out => C_out,

        pc_write_en  => pc_write_en,
        mem_write_en => mem_write_en,
        reg_write_en => reg_write_en,

        jump_to_out  => jump_to_out,
        mem_off_out  => mem_off_out,
        opcode_out   => opcode_out
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

    -- Testwerte

    enable <= '0';
    wait for 100 ns;

    jump_to_in <=  "00000000000000001111111100000000";

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

