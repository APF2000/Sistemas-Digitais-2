library ieee;
use ieee.numeric_bit.all;

entity alucontrol is
  port (
    aluop: in bit_vector(1 downto 0);
    opcode : in bit_vector(10 downto 0);
    aluCtrl : out bit_vector(3 downto 0)
  );
end entity;

architecture arc of alucontrol is
  signal auxop : bit_vector(3 downto 0);
  begin
    with aluop select
      aluCtrl <= "0010" when "00",
                 "0111" when "01",
                 auxop  when "10",
                 "1111" when others;

    with opcode select
      auxop <= "0010" when B"1000_1011_000", -- ADD
               "0110" when B"1100_1011_000", -- SUB
               "0000" when B"1000_1010_000", -- AND
               "0001" when B"1010_1010_000", -- ORR
               "1111" when others;
end architecture;
