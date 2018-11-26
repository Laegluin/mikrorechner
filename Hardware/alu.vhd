-- 32-Bit-ALU

-- entity ALU

-- architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity ALU is

    port
    (
        A, B        : in    unsigned(bit_Width-1 downto 0);    -- Operanden
        opcode      : in    unsigned(4 downto 0);              -- Opcode
        ALU_Out     : out   unsigned(bit_Width-1 downto 0);    -- Ausgang
        ALU_Flag    : out   std_logic                          -- Flag
     );
end entity ALU;

architecture behavior of ALU is

signal ALU_Result   : unsigned(bit_Width-1 downto 0);    -- Zwischenergebnis

begin
    process(A,B,opcode)
    begin
        case(opcode) 
            is
                -- Arithmetische Operationen mit Registern
            -- ADD
            when "00000" =>
                ALU_Result <= A + B;
            -- SUB
            when "10111" =>
                ALU_Result <= A - B;

            -- MUL
            when "01110" =>
                -- wandelt A und B in Dezimalzahlen um, multipliziert diese,
                -- und wandelt 32 Bits des Ergebnisses wieder in unsiged array um.
                ALU_Result <= to_unsigned((to_integer(A) * to_integer(B)), 32) ;
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
                -- A wird um B Bits nach links geshiftet;
                -- B muss zwischen 0 und 32 sein.
                -- seine höchsten Bits werden mit Nullen aufgefuellt.
            when "10100" =>
                ALU_Result <= shift_left(A, (to_integer(B)));
            --SHIFTR
                -- A wird um B Bit nach rechts geshiftet;
                -- B muss zwischen 0 und 32 sein.
                -- seine niedrigsten Bits werden mit Nullen aufgefuellt.
            when "10101" =>
                ALU_Result <= shift_right(A, (to_integer(B)));
            --SIGNED_SHIFTR
                -- A wird um B Bit nach rechts geshiftet;
                -- B muss zwischen 0 und 32 sein.
                -- seine niedrigsten Bits werden mit Nullen aufgefuellt.
                -- Ergebnis behaelt Vorzeichen von A bei.
            when "10110" =>
                ALU_Result <= unsigned(shift_right(signed(A), to_integer(B)));


                -- Vergleiche
            -- CMP_EQ
            when "00011" =>
                if (A = B) then
                    ALU_Flag <= '1';
                else
                    ALU_Flag <= '0';
                end if;
            -- CMP_GT
            when "00100" =>
                if (A > B) then
                    ALU_Flag <= '1';
                else
                    ALU_Flag <= '0';
                end if;
            -- CMP_GE
            when "00101" =>
                if (A >= B) then
                    ALU_Flag <= '1';
                else
                    ALU_Flag <= '0';
                end if;

            -- absturz vermeidung, falls fehler im opcode
            when others =>
                ALU_Result <= (others => '0');

        end case;

    end process;

    -- update ALU_Out Port
    ALU_Out <= ALU_Result;

end architecture behavior;
