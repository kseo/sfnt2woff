{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Graphics.WOFF 
  ( encode
  , EncodeResult
  , ErrorCode(OutOfMemory, Invalid, CompressionFailure, BadSignature, BufferToSmall, BadParameter, IllegalOrder)
  , Warning(..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Bits
import Data.Word
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc

#include <woff.h>

type EncodeResult = Either ErrorCode (ByteString, [Warning])

data ErrorCode =
    Ok
  | OutOfMemory
  | Invalid
  | CompressionFailure
  | BadSignature
  | BufferToSmall
  | BadParameter
  | IllegalOrder
  deriving (Eq, Show)

data Warning =
    UnknownVersion
  | ChecksumMismatch
  | MisalignedTable
  | TrailingData
  | UnpaddedTable
  | RemovedDSIG
  deriving (Eq, Show)

toWarnings :: CUInt -> [Warning]
toWarnings cWarnings = 
  let matchingFlags = filter (testFlag cWarnings) warningFlags
   in map (flagToWarning . fromIntegral) matchingFlags
  where
    testFlag x y = x .&. y /= 0

    warningFlags =
      [ #{const eWOFF_warn_unknown_version}
      , #{const eWOFF_warn_checksum_mismatch}
      , #{const eWOFF_warn_misaligned_table}
      , #{const eWOFF_warn_trailing_data}
      , #{const eWOFF_warn_unpadded_table}
      , #{const eWOFF_warn_removed_DSIG}
      ]

    flagToWarning #{const eWOFF_warn_unknown_version} = UnknownVersion
    flagToWarning #{const eWOFF_warn_checksum_mismatch} = ChecksumMismatch
    flagToWarning #{const eWOFF_warn_misaligned_table} = MisalignedTable
    flagToWarning #{const eWOFF_warn_trailing_data} = TrailingData
    flagToWarning #{const eWOFF_warn_unpadded_table} = UnpaddedTable
    flagToWarning #{const eWOFF_warn_removed_DSIG} = RemovedDSIG

toErrorCode :: CUInt -> ErrorCode
toErrorCode #{const eWOFF_ok} = Ok
toErrorCode #{const eWOFF_out_of_memory} = OutOfMemory
toErrorCode #{const eWOFF_invalid} = Invalid
toErrorCode #{const eWOFF_compression_failure} = CompressionFailure
toErrorCode #{const eWOFF_bad_signature} = BadSignature
toErrorCode #{const eWOFF_buffer_too_small} = BufferToSmall
toErrorCode #{const eWOFF_bad_parameter} = BadParameter
toErrorCode #{const eWOFF_illegal_order} = IllegalOrder

statusToErrorCodeAndWarnigns :: CUInt -> (ErrorCode, [Warning])
statusToErrorCodeAndWarnigns cStatus =
  (toErrorCode $ fromIntegral (cStatus .&. 0xff), toWarnings (clearBit cStatus 0xff))

encode :: ByteString -> Word16 -> Word16 -> IO EncodeResult
encode sfnt major minor =
  BS.unsafeUseAsCStringLen sfnt $ \(sfntData, sfntLen) -> (
  alloca $ \woffLenPtr -> (
  alloca $ \statusPtr -> do
    woffData <- cWoffEncode sfntData (CUInt (fromIntegral sfntLen)) (CUShort major) (CUShort minor) woffLenPtr statusPtr
    status <- peek statusPtr
    let (errorCode, warnings) = statusToErrorCodeAndWarnigns status

    case errorCode of
      Ok -> do
        woffLen <- peek woffLenPtr
        woff <- BS.unsafePackMallocCStringLen (woffData, fromIntegral woffLen)
        return $ Right (woff, warnings)
      _ -> return $ Left errorCode))

foreign import ccall unsafe "woff.h woffEncode"
  cWoffEncode :: CString
              -> CUInt
              -> CUShort
              -> CUShort
              -> Ptr CUInt
              -> Ptr CUInt
              -> IO CString

foreign import ccall unsafe "woff.h woffSetMetadata"
  cWoffSetMetadata :: CString
                   -> Ptr CUInt
                   -> CString
                   -> CUInt
                   -> Ptr CUInt
                   -> IO CString

foreign import ccall unsafe "woff.h woffSetPrivateData"
  cWoffSetPrivateData :: CString
                      -> Ptr CUInt
                      -> CString
                      -> CUInt
                      -> Ptr CUInt
                      -> IO CString
