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
        clk, enable  : in std_logic;
        reg_write_en : out std_logic; -- Schreibmodus fuer Register
        pc_write_en  : out std_logic; -- Schreibmodus fuer PC
        mem_write_en : out std_logic; -- Schreibmodus fuer Speicher


        instruction  : in unsigned(bit_Width-1 downto 0);
        opcode       : buffer unsigned(4 downto 0);
        A,B,C        : out unsigned(adr_Width-1 downto 0);
        reg_offset   : out unsigned(reg_offset_Bits-1 downto 0);
        jump_offset  : out unsigned(jump_offset_Bits-1 downto 0);
        mem_offset   : out unsigned(mem_offset_Bits-1 downto 0)
    );
end entity decoder;

architecture behavior of decoder is

begin
    process(clk,enable)
    begin
        if enable = '0' then
            opcode <= "01100"; --NOOP

        elsif rising_edge(clk) and enable = '1' then

            -- init output
            reg_write_en <= '0';
            pc_write_en  <= '0';
            mem_write_en <= '0';


            -- parse Befehl
            opcode      <= instruction(bit_Width-1 downto bit_Width-opcode_Bits);
            C           <= instruction(bit_Width-opcode_Bits-1 downto bit_Width-opcode_Bits-adr_Width);
            B           <= instruction(bit_Width-opcode_Bits-adr_Width-1 downto bit_Width-opcode_Bits-(2*adr_Width));
            A           <= instruction(bit_Width-opcode_Bits-(2*adr_Width)-1 downto bit_Width-opcode_Bits-(3*adr_Width));
            reg_offset  <= instruction(bit_Width-opcode_Bits-adr_Width-1 downto 0);
            jump_offset <= instruction(bit_Width-opcode_Bits-1 downto 0);
            mem_offset  <= instruction(bit_Width-opcode_Bits-(2*adr_Width)-1 downto 0);

            case opcode is
                    --Register
--                --COPY
--                when "00001" => 
--                    ? <= 
                --SET
                when "01010" =>
                    reg_write_en <= '1';

                  --Spruenge
                --JMP
                when "00110" =>
                    pc_write_en <= '1';
                --JMP_REL
                when "00111" =>
                    pc_write_en <= '1';
                --JMP_IF
                when "01000" =>
                    pc_write_en <= '1';
                --JMP_REL_IF
                when "01001" =>
                    pc_write_en <= '1';

                    --Speicher
--                --LOAD
--                when "01010" =>
--                    ? <=
                --STORE
                when "01011" =>
                     mem_write_en <= '1';

                    --Anderes
--                --NOOP
--                when "01100" =>
--                    ? <=
--                --HALT
--                when "01101" =>
--                    ? <=

                when others =>
                    reg_write_en <= '0';
                    pc_write_en  <= '0';
                    mem_write_en <= '0';

            end case;

        end if;

    end process;

end behavior;
