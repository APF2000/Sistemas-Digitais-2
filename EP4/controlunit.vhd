library ieee;
use ieee.numeric_bit.all;

entity controlunit is
  port (
    -- To datapath
    reg2loc      : out bit;
    uncondBranch : out bit;
    branch       : out bit;
    memRead      : out bit;
    memToReg     : out bit;
    aluOp        : out bit_vector(1 downto 0);
    memWrite     : out bit;
    aluSrc       : out bit;
    regWrite     : out bit;
    -- From datapath
    opcode       : in bit_vector(10 downto 0)
  );
end entity;

architecture arc of controlunit is
  signal Rformat, Dformat : bit_vector(10 downto 0);
  signal CBZformat, cbz : bit_vector(7 downto 0);
  signal Bformat, b : bit_vector(5 downto 0);
  signal isB, isCBZ, isR, isD, ldur, stur : bit;
  signal whichFormat : bit_vector(4 downto 0);
  signal selection : bit_vector(9 downto 0);
  begin
    Rformat <= opcode;
    with Rformat select
      isR <= '1' when B"1000_1011_000", -- ADD
             '1' when B"1100_1011_000", -- SUB
             '1' when B"1000_1010_000", -- AND
             '1' when B"1010_1010_000", -- ORR
             '0' when others;

    Dformat <= opcode;
    with Dformat select
      isD <= '1' when B"1111_1000_010", -- LDUR
             '1' when B"1111_1000_000", -- STUR
             '0' when others;

    CBZformat <= opcode(10 downto 3);
    isCBZ <= '1' when (CBZformat = cbz)
             else '0';
    cbz <= B"1011_0100";

    Bformat <= opcode(10 downto 5);
    isB <= '1' when (Bformat = b)
           else '0';
    b <= B"0001_01";

    ldur <= isD and Dformat(1);
    stur <= isD and not Dformat(1);
    whichFormat(0) <= isR;
    whichFormat(1) <= ldur;
    whichFormat(2) <= stur;
    whichFormat(3) <= isCBZ;
    whichFormat(4) <= isB;

    with whichFormat select
      selection <= B"0_0000_10_001" when "00001", -- R
                   B"0_0011_00_011" when "00010", -- LDUR
                   B"1_0000_00_110" when "00100", -- STUR
                   B"1_0100_01_000" when "01000", -- CBZ
                   B"0_1000_01_000" when "10000", -- B
                   B"0_0000_00_000" when others;

    reg2loc <= selection(9);
    uncondBranch <= selection(8);
    branch <= selection(7);
    memRead <= selection(6);
    memToReg <= selection(5);

    aluOp <= selection(4 downto 3);
    memWrite <= selection(2);
    aluSrc <= selection(1);
    regWrite <= selection(0);
end architecture;
