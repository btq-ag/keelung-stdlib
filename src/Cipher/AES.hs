module Cipher.AES where 

import Keelung



-- references
-- https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197.pdf
-- https://opensource.apple.com/source/CommonCrypto/CommonCrypto-55010/Source/AESedp/AES.c.auto.html

-- | Constants

-- | Number of columns (32-bit words) comprising the State. For this standard, Nb = 4.
nb :: Int
nb = 4

-- | KeyExpansion – round keys are derived from the cipher key using the AES key schedule. 
--   AES requires a separate 128-bit round key block for each round plus one more.
expandKey = undefined