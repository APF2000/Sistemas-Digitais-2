library ieee;
use ieee.numeric_bit.all;

entity signExtend is
  port(
    i: in bit_vector(31 downto 0);
    o: out bit_vector(63 downto 0)
  );
end entity signExtend;

architecture arc of signExtend is
  signal size : bit_vector(63 downto 0);
  signal aux, aux2 : bit_vector(2 downto 0);
  signal Dformat   : bit_vector(10 downto 0);
  signal CBZformat : bit_vector(7 downto 0);
  signal Bformat   : bit_vector(5 downto 0);
  begin
    Dformat <= i(31 downto 21);
    CBZformat <= i(31 downto 24);
    Bformat <= i(31 downto 26);

    o <= size;
    with aux select
      size <= bit_vector( resize(signed(Dformat), size'length) ) when "100",
              bit_vector( resize(signed(CBZformat), size'length) ) when "010",
              bit_vector( resize(signed(Bformat), size'length) ) when "001",
              (others => '1')  when others;

    with i(31 downto 26) select
      aux <= "001" when "000101",
             aux2 when others;

    with i(31 downto 24) select
      aux2 <= "010" when "10110100",
              "100" when others;

end architecture;
