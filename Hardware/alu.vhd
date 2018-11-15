-- 32-Bit-ALU

-- entity ALU

-- architecture

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ALU is
    generic
    (
        bit_Width   : integer := 32;
        opcode_Bits : integer := 6;
    );
    port
    (
        A, B        : in    std_logic_vector(bit_Width-1 downto 0);
        opcode      : in    std_logic_vector(opcode_Width-1 downto 0);
        ALU_Out     : out   std_logic_vector(bit_Width-1 downto 0);
        ALU_Flag    : out   std_logic
     );
end entity ALU;

architecture behavior of ALU is

signal ALU_Result   : std_logic_vector (bit_Width-1 downto 0)
signal temp_Result  : std_logic_vector (bit_Width-1 downto 0);

begin 
    process(A,B,opcode)
    begin
        case(opcode) is
                -- Arithmetische Operationen mit Registern
            -- ADD
            when "000000" => 
                ALU_Result <= A + B;
            -- SUB
            when "000001" => 
                ALU_Result <= A - B;

                -- Arithmetische Operationen mit Immediate-Werten
            -- ADDI
            when "000010" => 
                ALU_Result <= A + B;
            -- SUBI
            when "000011" => 
                ALU_Result <= A - B;

                -- Logische Operationen mit Registern
            -- AND
            when "000100" => 
                ALU_Result <= A and B;
            -- OR
            when "000101" => 
                ALU_Result <= A or B;
            -- NOT
            when "000110" => 
                ALU_Result <= not A;

                -- Schiebeoperationen
            -- LSL
            when "000111" => 
                ALU_Result <= not A;
            -- LSR
            when "001000" => 
                ALU_Result <= not A;
            -- ASR
            when "001001" => 
                ALU_Result <= not A;

            -- LSLI
            when "001010" => 
                ALU_Result <= not A;
            -- LSRI
            when "001011" => 
                ALU_Result <= not A;
            -- ASRI
            when "001100" => 
                ALU_Result <= not A;

                -- Vergleiche
            -- CMPE
            when "001101" => 
                ALU_Flag <= A + B;
            -- CMPGT
            when "001110" => 
                ALU_Flag <= A + B;
            -- CMPLT
            when "001111" => 
                ALU_Flag <= A + B;

        end case;

    end process;

end architecture behavior;
