-- Execute-Stage of 32-Bit-CPU

-- contains jump logic and ALU control

-- entity executor

-- architecture behavior

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.universal_constants.all;

entity executer is

    port
    (
        clk, enable   : in       std_logic;
        pc_in         : in       unsigned(bit_Width-1 downto 0);
        jump_off_in   : in       signed(bit_Width-1 downto 0);
        mem_off_in    : in       unsigned(bit_Width-1 downto 0);
        reg_imm       : in       unsigned(bit_Width-1 downto 0);
        opcode_in     : in       unsigned(opcode_Bits-1 downto 0);
        alu_flag      : in       std_logic;
        jump_to_in    : in       unsigned(bit_Width-1 downto 0);
        C_in          : in       unsigned(bit_Width-1 downto 0);

        C_out         : out      unsigned(bit_Width-1 downto 0);
        pc_write_en   : out      std_logic;
        mem_write_en  : out      std_logic;
        reg_write_en  : out      std_logic;
        jump_to_out   : out      unsigned(bit_Width-1 downto 0);
        mem_off_out   : out      unsigned(bit_Width-1 downto 0);
        opcode_out    : out      unsigned(bit_Width-1 downto 0)
    );
end entity executer;

architecture behavior of executer is

    -- signale


begin
    process(clk, enable)
    begin
        if rising_edge(clk) and enable = '1' then

            -- init output
            reg_write_en <= '0';
            pc_write_en  <= '0';
            mem_write_en <= '0';
            C_out        <= C_in;




            case opcode_in is
                    --Register
--                --COPY
--                when "00001" =>·
--                    ? <=·
                --SET
                when "01010" =>
                   reg_write_en <= '1';

                  --ALU-Operationen
                -- bei denen Write-Back des Ergebnisses noetig ist
                   -- Arithmetische Operationen
                when "00000" | "10111" | "01110" | "01111"
                   -- Logische Operationen
                   | "10000" | "10001" | "10010" | "10011"
                   -- shifts
                   | "10100" | "10101" | "10110" 
                   => reg_write_en <= '1';

                  --Spruenge
                 --JMP
                when "00110" =>
                    jump_to_out <= jump_to_in;
                    pc_write_en <= '1';
                --JMP_REL
                when "00111" =>
                    jump_to_out <= unsigned(signed(pc_in) + jump_off_in);
                    pc_write_en <= '1';
                --JMP_IF
                when "01000" =>
                    if alu_flag = '1' then
                        jump_to_out <= jump_to_in;
                        pc_write_en <= '1';
                    end if;
                --JMP_REL_IF
                when "01001" =>
                    if alu_flag = '1' then
                        jump_to_out <= unsigned(signed(pc_in) + jump_off_in);
                        pc_write_en <= '1';
                    end if;
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

        else
            -- noop
        end if;

    end process;

end behavior;
