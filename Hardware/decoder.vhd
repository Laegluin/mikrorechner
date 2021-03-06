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
        reset         : in       std_logic;
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
signal sign_temp        : unsigned(0 downto 0) := "0";

signal reg_imm_ext      : unsigned(bit_Width - reg_offset_Bits -1 downto 0)  := (others => '0');
signal jump_offset_ext0 : unsigned(bit_Width - jump_offset_Bits -1 downto 0) := (others => '0');
signal Jump_offset_ext1 : unsigned(bit_Width - jump_offset_Bits -1 downto 0) := (others => '1');

signal mem_offset_ext   : unsigned(bit_Width - mem_offset_Bits -1 downto 0)  := (others => '0');

signal tmp_pc           : unsigned(bit_Width-1 downto 0) := (others => '0');

begin
    process(clk,enable)
    begin

        -- init values
        -- sign_temp <= "0"; should only change on rising edge if at all

        -- reset
        if reset = '1' then
            -- reset out ports
            tmp_pc         <= (others => '0');
            opcode         <= (others => '1');
            A              <= (others => '0');
            B              <= (others => '0');
            C              <= (others => '0');
            reg_imm        <= (others => '0');
            jump_offset    <= (others => '0');
            mem_offset     <= (others => '0');
            reg_offset_en  <= '0';
            -- reset signals
            sign_temp      <= "0";


        -- regular work mode
        elsif rising_edge(clk) then
            if enable = '1' then

            -- init output
            tmp_pc <= pc_in;

            -- parse Befehl
            opcode      <= instruction(bit_Width-1 downto bit_Width-opcode_Bits);

            C           <= instruction(bit_Width-opcode_Bits-1 downto bit_Width-opcode_Bits-adr_Width);
            A           <= instruction(bit_Width-opcode_Bits-adr_Width-1 downto bit_Width-opcode_Bits-(2*adr_Width));
            B           <= instruction(bit_Width-opcode_Bits-(2*adr_Width)-1 downto bit_Width-opcode_Bits-(3*adr_Width));

            reg_imm     <= reg_imm_ext & instruction(bit_Width-opcode_Bits-adr_Width-1 downto 0);

            sign_temp   <= instruction(bit_Width-opcode_Bits-1 downto bit_Width-opcode_Bits-1);

            -- bandaid fix for timing issue, seems like using the sign_temp signal delays by one cycle
            --jump_offset <= instruction(bit_Width-opcode_Bits-1 downto bit_Width-opcode_Bits-1) & instruction(bit_Width-opcode_Bits-1 downto bit_Width-opcode_Bits-1) & instruction(bit_Width-opcode_Bits-1 downto bit_Width-opcode_Bits-1) & instruction(bit_Width-opcode_Bits-1 downto bit_Width-opcode_Bits-1) & instruction(bit_Width-opcode_Bits-1 downto bit_Width-opcode_Bits-1) & instruction(bit_Width-opcode_Bits-1 downto 0);

            if instruction(bit_Width-opcode_Bits-1 downto bit_Width-opcode_Bits-1) = "0" then
                jump_offset <= jump_offset_ext0 & instruction(bit_Width-opcode_Bits-1 downto 0);
            elsif instruction(bit_Width-opcode_Bits-1 downto bit_Width-opcode_Bits-1) = "1" then
                --jump_offset <= jump_offset_ext1 & not instruction(bit_Width-opcode_Bits-1 downto 0) + "1";
                jump_offset <= jump_offset_ext1 & instruction(bit_Width-opcode_Bits-1 downto 0);
                -- jump_offset <= temp + "1";
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

    pc_out <= tmp_pc;

end behavior;
