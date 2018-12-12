-- Instruction-Decoder

-- entity decoder

-- architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity decoder is

    port
    (
        clk, enable  : in       std_logic;
        instruction  : in       unsigned(bit_Width-1 downto 0);
        pc_in        : in       unsigned(bit_Width-1 downto 0);

        pc_out       : out      unsigned(bit_Width-1 downto 0);
        opcode       : out      unsigned(4 downto 0);
        alu_opc      : out      unsigned(4 downto 0);    -- spart man sich nen mux
        A,B,C        : out      unsigned(adr_Width-1 downto 0);
        reg_imm      : out      unsigned(bit_Width-1 downto 0);
        jump_imm     : out      unsigned(adr_Width-1 downto 0);  -- auch hier mux gespart
        jump_offset  : out      signed(bit_Width-1 downto 0);
        mem_offset   : out      unsigned(bit_Width-1 downto 0)
    );

end entity decoder;

architecture behavior of decoder is

    -- sign extention
signal reg_imm_ext     : unsigned(bit_Width - reg_offset_Bits -1 downto 0)  := (others => '0');
signal jump_offset_ext : signed(bit_Width - jump_offset_Bits -2 downto 0) := (others => '0');
signal mem_offset_ext  : unsigned(bit_Width - mem_offset_Bits -1 downto 0)  := (others => '0');


begin
    process(clk,enable)
    begin

        if rising_edge(clk) then
            if enable = '1' then

            -- init output
            pc_out <= pc_in;

            -- parse Befehl
            opcode      <= instruction(bit_Width-1 downto bit_Width-opcode_Bits);

            alu_opc     <= instruction(bit_Width-1 downto bit_Width-opcode_Bits);
            C           <= instruction(bit_Width-opcode_Bits-1 downto bit_Width-opcode_Bits-adr_Width);
            B           <= instruction(bit_Width-opcode_Bits-adr_Width-1 downto bit_Width-opcode_Bits-(2*adr_Width));
            A           <= instruction(bit_Width-opcode_Bits-(2*adr_Width)-1 downto bit_Width-opcode_Bits-(3*adr_Width));
            reg_imm     <= reg_imm_ext & instruction(bit_Width-opcode_Bits-adr_Width-1 downto 0);
            jump_offset <= jump_offset_ext & signed(instruction(bit_Width-opcode_Bits-1 downto 1)) & signed(instruction(1 downto 0));
            mem_offset  <= mem_offset_ext & instruction(bit_Width-opcode_Bits-(2*adr_Width)-1 downto 0);
            jump_imm    <= instruction(bit_Width-opcode_Bits-1 downto bit_Width-opcode_Bits-adr_Width);

            end if;
        else
            -- noop
        end if;

    end process;

end behavior;
