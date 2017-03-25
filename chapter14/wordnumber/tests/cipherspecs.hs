module CipherSpecs where

import Test.QuickCheck
import VigenereCipher (encode, decode)

-- TODO: It does not work for "a\NAK" find out why
vigenereSpec :: String -> Bool
vigenereSpec x = x == (decode (encode x))

cipherSpec :: IO ()
cipherSpec = do
  quickCheck vigenereSpec
