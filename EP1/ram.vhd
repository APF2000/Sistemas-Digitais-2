library ieee;
use ieee.numeric_bit.all;


entity ram is
  generic (
    addressSize : natural := 64;
    wordSize    : natural := 32
  );
  port(
    -- wr -> write = '1', read = '0'
    ck, wr : in  bit;
    addr   : in  bit_vector(addressSize-1 downto 0);
    data_i : in  bit_vector(wordSize-1 downto 0);
    data_o : out bit_vector(wordSize-1 downto 0)
  );
end ram;

architecture arc of ram is

  type memory is array (2**addressSize-1 downto 0)
      of bit_vector(wordSize-1 downto 0);
  signal chip : memory;

begin
  process(ck, wr, addr, data_i)
    begin
      if rising_edge(ck) then
        -- write
        if wr = '1' then
          chip(to_integer(unsigned(addr))) <= data_i;
        else
        end if;
      else
      end if;
  end process;
  
  -- read
  data_o <= chip(to_integer(unsigned(addr)));

end arc;
