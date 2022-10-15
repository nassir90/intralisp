-- No loops? No problem!
use library ieee;
use ieee.std_logic_1164.all;
entity decoder_5to32 is
  port ([%dotimes (i 5)
        [@
        A[%=i%] : in std_logic;
        @]%][%dotimes (i 32)
        [@
        O[%=i%] : out std_logic;
        @]%]);
end decoder_5to32;
architecture decoder_5to32_impl of decoder_5to32 is
begin
[% dotimes (i (expt 2 5))[@
        O[%=i%] <= ([% dotimes (j 5)
        (when (= 0 (logand (expt 2 j) i)) [@not @])
        (format t "A~d" j)
        (unless (= j 4) [@ and @])%]);
@]%]
end decoder_5to32_impl;
