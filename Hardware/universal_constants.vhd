-- universal constants used in the 32-Bit-CPU

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package universal_constants is

    constant bit_Width          : integer   := 32; -- Wortbreite
    constant opcode_Bits        : integer   := 5;  -- Opcode-Bitumfang
    constant adr_Width          : integer   := 6;  -- Bitumfang der Registeradressen

    constant reg_offset_Bits    : integer   := 21; -- Register Offset Bitumfang
    constant jump_offset_Bits   : integer   := 27; -- Jump Offset Bitumfang
    constant mem_offset_Bits    : integer   := 15; -- Speicher Offset Bitumfang

end universal_constants;
