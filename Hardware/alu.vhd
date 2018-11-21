-- 32-Bit-ALU

-- entity ALU

-- architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ALU is
    generic
    (
        bit_Width   : integer                        := 32; -- Wortbreite
        opcode_Bits : integer                        := 5;  -- Opcode-Bitumfang
--        shift_Zeros : unsigned(bit_Width-1 downto 0) := "00000000000000000000000000000000" -- Nullen, die beim Shiften aufgefuellt werden
        shift_Ones  : unsigned(dit_Width-1 downto 0) := "11111111111111111111111111111111" -- Einsen, die beim Shiften aufgefuellt werden
    );
    port
    (
        A, B        : in    unsigned(bit_Width-1 downto 0);    -- Operanden
        opcode      : in    unsigned(opcode_Width-1 downto 0); -- Opcode
        ALU_Out     : out   unsigned(bit_Width-1 downto 0);    -- Ausgang
        ALU_Flag    : out   std_logic                          -- Flag
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

--                -- Schiebeoperationen
--            -- SHIFTL
--                -- A wird um 1 Bit nach links geshiftet;
--                -- sein höchstes Bit wird um niedrigstes Bit von B aufgefuellt.
--            when "10100" => 
--                ALU_Result <= A(bit_Width-1 downto 1) & B(1 downto 0);
--                -- A wird um 1 Bit nach rechts geshiftet;
--                -- sein niedrigstes Bit wird um höchstes Bit von B aufgefuellt.
--            -- SHIFTR
--           when "10101" => 
--                ALU_Result <= B(bit_Width-1 downto bit_Width-2) & A(bit_Width-2 downto 0);
--                -- A wird um 1 Bit nach rechts geshiftet;
--                -- sein zweitniedrigstes Bit wird um höchstes Bit von B aufgefuellt;
--                -- behaelt somit Vorzeichenbit von A bei;
--                -- Vorrausgesetzt, das Vorzeichen ist im niedrigsten Bit codiert.
--            -- SIGNED_SHIFTR
--            when "10110" => 
--                ALU_Result <= B(bit_Width-1 downto bit_Width-2) & A(bit_Width-2 downto 1) & A(1 downto 0);

                -- Schiebeoperationen (die wohl bessere Alternative)
            -- SHIFTL
                -- A wird um B Bits nach links geshiftet;
                -- B muss zwischen 0 und 32 sein.
                -- seine höchsten Bits werden mit 1en aufgefuellt.
            when "10100" => 
                ALU_Result <= A(bit_Width-1 downto to_integer(B)) & shift_Ones(to_integer(B) downto 0);
                -- A wird um B Bit nach rechts geshiftet;
                -- B muss zwischen 0 und 32 sein.
                -- seine niedrigsten Bits werden mit 1en aufgefuellt.
                ALU_Result <= A(bit_Width-1 downto 1) & B(1 downto 0);
                -- A wird um 1 Bit nach rechts geshiftet;
                -- sein niedrigstes Bit wird um höchstes Bit von B aufgefuellt.
            -- SHIFTR
            when "10101" => 
                ALU_Result <= shift_Ones(bit_Width-to_integer(B) downto 0) & A(bit_Width-to_integer(B) downto 0);
                -- A wird um B Bit nach rechts geshiftet;
                -- B muss zwischen 0 und 31 sein.
                -- seine Bits angefangen mit dem zweitniedrigsten Bit werden mit 1en aufgefuellt;
                -- behaelt somit Vorzeichenbit von A bei;
                -- Vorrausgesetzt, das Vorzeichen ist im niedrigsten Bit codiert.
            -- SIGNED_SHIFTR
            when "10110" => 
                ALU_Result <= shift_Ones(bit_Width-to_integer(B) downto 0) & A(bit_Width-to_integer(B) downto 1) & A(1 downto 0);

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
