{-# LANGUAGE OverloadedStrings #-}

module MailToMe (mailTo) where

import Data.Maybe (fromMaybe)
import Data.URLEncoded (importString, (%!))
import Network.Mail.SMTP (Address(..), sendMail, simpleMail, plainTextPart)

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

mailTo :: String -> BSC.ByteString -> IO ()
mailTo to dat = do
	ue <- importString $ BSC.unpack dat
	return () `fromMaybe` do
		name <- ue %! ("name" :: String)
		addr <- ue %! ("address" :: String)
		body <- ue %! ("body" :: String)
		return . sendMail "skami.iocikun.jp" . simpleMail
			(Address (Just "Homepage") "tatsuya@skami.iocikun.jp")
			[Address (Just "Administrator") (T.pack to)] [] []
			(T.pack name) . (: []) . plainTextPart . LT.pack $
				"お名前: " ++ name ++
				"\nメールアドレス: " ++ addr ++
				"\n\n内容:\n" ++ body
