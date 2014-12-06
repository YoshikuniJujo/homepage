{-# LANGUAGE OverloadedStrings #-}

module MailToMe (mailFromBS) where

import Data.Maybe (fromMaybe)
import Data.URLEncoded (importString, (%!))
import Network.Mail.SMTP (Address(..), sendMail, simpleMail, plainTextPart)

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

mailFromBS :: String -> BSU.ByteString -> IO ()
mailFromBS addr r = do
	ue <- importString $ BSC.unpack r
	fromMaybe (return ()) $ do
		nm <- ue %! ("name" :: String)
		uaddr <- ue %! ("address" :: String)
		bdy <- ue %! ("body" :: String)
		return $ mailTo "Administer" (T.pack addr) (T.pack nm)
			. LT.pack $ "お名前: " ++ nm
				++ "\nメールアドレス: " ++ uaddr
				++ "\n\n内容:\n" ++ bdy

mailTo :: T.Text -> T.Text -> T.Text -> LT.Text -> IO ()
mailTo n a t b = sendMail "skami.iocikun.jp" $ simpleMail
	(Address (Just "Homepage") "tatsuya@skami.iocikun.jp")
	[Address (Just n) a] [] [] t [plainTextPart b]
