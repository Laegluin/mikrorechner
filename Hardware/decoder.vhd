-- Instruction-Decoder

-- entity decoder

-- architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity decoder is
    generic
    (
        bit_Width           : integer   := 32; -- Wortbreite
        opcode_Bits         : integer   := 5;  -- Opcode-Bitumfang
        register_Bits       : integer   := 6;  -- Register-Adressbreite
        reg_offset_Bits     : integer   := 21; -- Register Offset Bitumfang
        jump_offset_Bits    : integer   := 27; -- Jump Offset Bitumfang
        mem_offset_Bits     : integer   := 15  -- Speicher Offset Bitumfang
    );
    port
    (
        clk, enable  : in std_logic;
        reg_write_en : out std_logic; -- Schreibmodus fuer Register
        pc_write_en  : out std_logic; -- Schreibmodus fuer PC
        mem_write_en : out std_logic; -- Schreibmodus fuer Speicher


        instruction  : in unsigned(bit_Width-1 downto 0);
        opcode       : out unsigned(opcode_Bits-1 downto 0);
        A,B,C        : out unsigned(register_Bits-1 downto 0);
        reg_offset   : out unsigned(reg_offset_Bits-1 downto 0);
        jump_offset  : out unsigned(jump_offset_Bits-1 downto 0);
        mem_offset   : out unsigned(mem_offset_Bits-1 downto 0)
    );
end entity decoder;

architecture behavior decoder is

begin
    process(clk)
    begin
        if rising_edge(clk) and enable = '1' then

            -- parse Befehl
            opcode      <= instruction(bit_Width-1 downto bit_Width-opcode_Bits-1);
            A           <= instruction(bit_Width-opcode_Bits-1 downto bit_Width-opcode_Bits-register_Bits-1);
            B           <= instruction(bit_Width-opcode_Bits-register_Bits-1 downto bit_Width-opcode_Bits-(2*register_Bits)-1);
            C           <= instruction(bit_Width-opcode_Bits-(2*register_Bits)-1 downto bit_Width-opcode_Bits-(3*register_Bits-register_Bits)-1);
            reg_offset  <= instruction(bit_Width-opcode_Bits-register_Bits-1 downto 0);
            jump_offset <= instruction(bit_Width-opcode_Bits-1 downto 0);
            mem_offset  <= instruction(bit_Width-opcode_Bits-(2*register_Bits)-1 downto 0);

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
