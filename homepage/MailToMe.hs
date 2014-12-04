{-# LANGUAGE OverloadedStrings #-}

module MailToMe (mailFromBS) where

import Data.URLEncoded (URLEncoded, importString, (%!))
import Network.Mail.SMTP (Address(..), sendMail, simpleMail, plainTextPart)

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

mailFromBS :: String -> BSU.ByteString -> IO ()
mailFromBS addr r = do
	ue <- importString $ BSC.unpack r
	maybe (return ()) (\nazo -> do
			putStrLn nazo
			mailTo "Administer" (T.pack addr) "nazo" $ LT.pack nazo
		) $ makeMailBody ue

makeMailBody :: URLEncoded -> Maybe String
makeMailBody ue =
	case (ue %! ("name" :: String), ue %! ("address" :: String),
			ue %! ("body" :: String)) of
		(Just nm, Just addr, Just bdy) -> Just $ "お名前: " ++ nm ++
			"\nメールアドレス: " ++ addr ++ "\n\n内容:\n" ++ bdy
		_ -> Nothing

mailTo :: T.Text -> T.Text -> T.Text -> LT.Text -> IO ()
mailTo n a t b = sendMail "skami.iocikun.jp" $ simpleMail
	(Address (Just "Homepage") "tatsuya@skami.iocikun.jp")
	[Address (Just n) a] [] [] t [plainTextPart b]
