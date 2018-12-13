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
signal vorz_tmp     : unsigned(0 downto 0);


begin
    process(A,B,opcode)

    begin

        vorz_tmp <= A(bit_Width-1 downto bit_Width-1);

        case(opcode) 
            is

                -- Register-Operationen
            -- COPY
                -- Wert aus Register B kopiert nach Register C
            when "00001" =>
                ALU_Result <= B;

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
                -- temporaeres workaround fuer division durch null
                if (B = 0) then
                    ALU_Result <= B;
                else
                    ALU_Result <= A / B;
                end if;

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
                -- seine niedrigsten Bits werden mit Nullen aufgefuellt.
            when "10100" =>
                ALU_Result <= shift_left(A, (to_integer(B)));
            --SHIFTR
                -- A wird um B Bit nach rechts geshiftet;
                -- B muss zwischen 0 und 32 sein.
                -- seine höchsten Bits werden mit Nullen aufgefuellt.
            when "10101" =>
                ALU_Result <= shift_right(A, (to_integer(B)));
            --SIGNED_SHIFTR
                -- A wird um B Bit nach rechts geshiftet;
                -- A wird als signed interpretiert.
                -- B muss zwischen 0 und 32 sein.
                -- seine höchsten Bits werden mit Nullen aufgefuellt.
                -- Ergebnis behaelt Vorzeichen von A bei.
                -- Vorzeichen ist im MSB kodiert (höchstes Bit).
            when "10110" =>
                    if (vorz_tmp = 1) then
                        ALU_Result <= A(bit_Width-1 downto bit_Width-1) & shift_right((not(A(bit_Width-2 downto 0)) + 1), to_integer(B));
                    else
                        ALU_Result <= A(bit_Width-1 downto bit_Width-1) & shift_right(A(bit_Width-2 downto 0), to_integer(B));
                    end if;

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
