library ieee;
use ieee.numeric_bit.all;
use std.textio.all;

entity rom is
  generic(
    addressSize : natural := 64;
    wordSize    : natural := 32;
    mifFileName : string  := "rom.dat"
  );
  port(
    addr : in  bit_vector(addressSize-1 downto 0);
    data : out bit_vector(wordSize-1 downto 0)
  );
end rom;

architecture arc of rom is

  constant address : natural := 2**addressSize;
  type memory is array (0 to address-1)
      of bit_vector(wordSize-1 downto 0);

  impure function init_mem(nameOfFile : in string) return memory is
      file mif_file : text open read_mode is nameOfFile;
      variable mif_line : line;
      variable temp_bv : bit_vector(wordSize-1 downto 0);
      variable temp_mem : memory;
  begin
      for i in memory'range loop
          readline(mif_file, mif_line);
          read(mif_line, temp_bv);
          temp_mem(i) := temp_bv;
      end loop;
      return temp_mem;
  end;

  constant mem : memory := init_mem(mifFileName);

  begin
    data <= mem(to_integer(unsigned(addr)));

end arc;
