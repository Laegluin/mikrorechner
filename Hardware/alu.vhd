-- 32-Bit-ALU

-- entity ALU

-- architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ALU is
    generic
    (
        bit_Width   : integer := 32; -- Wortbreite
        opcode_Bits : integer := 5;  -- Opcode-Bitumfang
    );
    port
    (
        A, B        : in    unsigned(bit_Width-1 downto 0);    -- Operanden
        opcode      : in    unsigned(opcode_Width-1 downto 0); -- Opcode
        ALU_Out     : out   unsigned(bit_Width-1 downto 0);    -- Ausgang
        ALU_Flag    : out   std_logic                        -- Flag
     );
end entity ALU;

architecture behavior of ALU is

signal ALU_Result   : unsigned(bit_Width-1 downto 0) -- Zwischenergebnis

begin
    process(A,B,opcode)
    begin
        case(opcode) is
                -- Arithmetische Operationen mit Registern
            -- ADD
            when "00000" => 
                ALU_Result <= A + B;
            -- SUB
            when "10111" => 
                ALU_Result <= A - B;

            -- MUL
            when "01110" => 
                ALU_Result <= A * B;
            -- DIV
            when "01111" => 
                ALU_Result <= A / B;

                -- Arithmetische Operationen mit Immediate-Werten
--            -- ADDI
--            when "00010" => 
--               ALU_Result <= A + B;
--            -- SUBI
--            when "00011" => 
--                ALU_Result <= A - B;

                -- Logische Operationen mit Registern
            -- AND
            when "10000" => 
                ALU_Result <= A and B;
            -- OR
            when "10001" => 
                ALU_Result <= A or B;
            -- NOT
            when "10010" => 
                ALU_Result <= not A;
            -- XOR
            when "10011" => 
                ALU_Result <= A xor B;

                -- Schiebeoperationen
            -- SHIFTL
                -- A wird um 1 Bit nach links geshiftet;
                -- sein höchstes Bit wird um niedrigtes Bit von B aufgefuellt.
            when "10100" => 
                ALU_Reslut <= A(bit_Width-1 downto 1) & B(1 downto 0);
                -- A wird um 1 Bit nach rechts geshiftet;
                -- sein niedrigstes Bit wird um höchstes Bit von B aufgefuellt.
            -- SHIFTR
            when "10101" => 
                ALU_Result <= B(bit_Width-1 downto bit_Width-2) & A(bit_Width-2 downto 0);
                -- A wird um 1 Bit nach rechts geshiftet;
                -- sein zweitniedrigstes Bit wird um höchstes Bit von B aufgefuellt;
                -- behaelt somit Vorzeichenbit von A bei;
                -- Vorrausgesetzt, das Vorzeichen ist im niedrigtsen Bit codiert.
            -- SIGNED_SHIFTR
            when "10110" => 
                ALU_Result <= B(bit_Width-1 downto bit_Width-2) & A(bit_Width-2 downto 1) & A(1 downto 0);

                -- Vergleiche
            -- CMP_EQ
            when "00011" => 
                if (A = B) then
                    ALU_Flag <= 1;
                else
                    ALU_Flag <= 0;
                end if;
            -- CMP_GT
            when "00100" => 
                 if (A > B) then
                    ALU_Flag <= 1;
                else
                    ALU_Flag <= 0;
                end if;
            -- CMP_GE
            when "00101" => 
                 if (A >= B) then
                    ALU_Flag <= 1;
                else
                    ALU_Flag <= 0;
                end if;

        end case;

    end process;

    -- update ALU_Out Port
    ALU_Out <= ALU_Result;

end architecture behavior;
