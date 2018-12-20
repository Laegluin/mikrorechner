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

end entity decoder;

architecture behavior of decoder is

    -- sign extention
signal sign_temp        : unsigned(bit_Width-1 downto bit_Width-2); 

signal reg_imm_ext      : unsigned(bit_Width - reg_offset_Bits -1 downto 0)  := (others => '0');
signal jump_offset_ext0 : unsigned(bit_Width - jump_offset_Bits -1 downto 0) := (others => '0');
signal Jump_offset_ext1 : unsigned(bit_Width - jump_offset_Bits -1 downto 0) := (others => '1');

signal mem_offset_ext   : unsigned(bit_Width - mem_offset_Bits -1 downto 0)  := (others => '0');


begin
    process(clk,enable)
    begin

        if rising_edge(clk) then
            if enable = '1' then

            -- init output
            pc_out <= pc_in;

            -- parse Befehl
            opcode      <= instruction(bit_Width-1 downto bit_Width-opcode_Bits);

            C           <= instruction(bit_Width-opcode_Bits-1 downto bit_Width-opcode_Bits-adr_Width);
            A           <= instruction(bit_Width-opcode_Bits-adr_Width-1 downto bit_Width-opcode_Bits-(2*adr_Width));
            B           <= instruction(bit_Width-opcode_Bits-(2*adr_Width)-1 downto bit_Width-opcode_Bits-(3*adr_Width));

            reg_imm     <= reg_imm_ext & instruction(bit_Width-opcode_Bits-adr_Width-1 downto 0);

            sign_temp <= instruction(bit_Width-opcode_Bits-1 downto bit_Width-opcode_Bits-2);

            if sign_temp = "0" then
                jump_offset <= jump_offset_ext0 & instruction(bit_Width-opcode_Bits-1 downto 0);
            elsif sign_temp = "1" then
                jump_offset <= jump_offset_ext1 & not instruction(bit_Width-opcode_Bits-1 downto 0) + "1";
            else jump_offset <= (others => '0');
            end if;

            mem_offset  <= mem_offset_ext & instruction(bit_Width-opcode_Bits-(2*adr_Width)-1 downto 0);

            -- absolute jumps, load, store => steuersignal an registerbank
            case (opcode) is

                when "00110" | "01000" | "01010" | "01011" =>
                reg_offset_en <= '1';
                when others =>
                reg_offset_en <= '0';

            end case;

            end if;
        else
            -- noop
        end if;

    end process;

end behavior;
