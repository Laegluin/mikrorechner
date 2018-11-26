-- universal constants used in the 32-Bit-CPU

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package universal_constants is

    constant bit_Width   : integer                        := 32; -- Wortbreite
    constant opcode_Bits : integer                        := 5;  -- Opcode-Bitumfang
end universal_constants;
