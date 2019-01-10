-- testbench for 32-Bit-Decoder

-- entity test_decoder

-- architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity test_decoder is

end test_decoder;

architecture behavior of test_decoder is
    -- Unit Under Test (UUT)
    component decoder

    port
    (
        clk, enable   : in       std_logic;
        instruction   : in       unsigned(bit_Width-1 downto 0);
        pc_in         : in       unsigned(bit_Width-1 downto 0);

        pc_out        : out      unsigned(bit_Width-1 downto 0);
        opcode        : buffer   unsigned(4 downto 0);
        A,B,C         : out      unsigned(adr_Width-1 downto 0);
        reg_imm       : out      unsigned(bit_Width-1 downto 0);
        jump_offset   : out      unsigned(bit_Width-1 downto 0);
        mem_offset    : out      unsigned(bit_Width-1 downto 0);
        reg_offset_en : out      std_logic
    );

    end component decoder;

    -- inputs
    signal clk, enable   : std_logic;
    signal instruction   : unsigned(bit_Width-1 downto 0);
    signal pc_in         : unsigned(bit_Width-1 downto 0);

    -- outputs
    signal pc_out        : unsigned(bit_Width-1 downto 0);

    signal A, B, C       : unsigned(adr_Width-1 downto 0);
    signal reg_imm       : unsigned(bit_Width-1 downto 0);
    signal jump_offset   : unsigned(bit_Width-1 downto 0);
    signal mem_offset    : unsigned(bit_Width-1 downto 0);
    signal reg_offset_en : std_logic;

    signal opcode        : unsigned(opcode_Bits-1 downto 0);

begin
    -- instanziiere UUT
    uut: decoder port map
    (
        clk           => clk,
        enable        => enable,

        pc_in         => pc_in,
        pc_out        => pc_out,
        instruction   => instruction,

        A             => A,
        B             => B,
        C             => C,

        reg_imm       => reg_imm,
        jump_offset   => jump_offset,
        mem_offset    => mem_offset,
        reg_offset_en => reg_offset_en,

        opcode        => opcode
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


    -- init
    enable <= '0';
    pc_in  <= (others => '0');
    pc_out <= (others => '0');
    instruction <= (others => '0');

    -- Testwerte
    
    wait for 10 ns;    
    enable <= '1';

        -- ADD reg3 reg2 reg
    instruction  <= "00000000011000010000001000000000";
    wait for 100 ns;

        -- AND reg3 reg2 reg1
    instruction  <= "10000000011000010000001000000000";
    wait for 100 ns;

        -- NOT reg2 reg1
    instruction  <= "10010000010000001000000000000000";
    wait for 100 ns;

        -- COPY reg2 reg1
    instruction  <= "00001000001000010000000000000000";
    wait for 100 ns;

        -- CMP_EQ reg2 reg1
    instruction  <= "00011000000000010000001000000000";
    wait for 100 ns;

        -- JMP reg1
    instruction  <= "00110000001000000000000000000000";
    wait for 100 ns;

        -- JMP_REL imm
    instruction  <= "00111000000000000000000000000001";
    wait for 100 ns;

        -- LOAD reg2 reg1 imm
    instruction  <= "01010000010000001000000000000001";
    wait for 100 ns;

        -- STORE reg2 reg1 imm
    instruction  <= "01011000010000001000000000000001";
    wait for 100 ns;



        -- test reset
    enable <= '0';
    instruction  <= "01010000010000001000000000000001";
    wait for 100 ns;



    assert False report "Ende des Tests erreicht.";
    wait;

    end process;


end architecture behavior;

