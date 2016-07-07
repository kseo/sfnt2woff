{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Graphics.WOFF 
  ( encode
  , EncodeResult
  , ErrorCode(OutOfMemory, Invalid, CompressionFailure, BadSignature, BufferToSmall, BadParameter, IllegalOrder)
  , Warning(..)
  , setMetadata
  , setPrivateData
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
import Foreign.Marshal.Utils

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
  deriving (Eq)

instance Show ErrorCode where
  show Ok = "ok"
  show OutOfMemory = "memory allocation failure"
  show Invalid = "invalid input font"
  show CompressionFailure = "zlib compression/decompression failure"
  show BadSignature = "incorrect WOFF file signature"
  show BufferToSmall = "buffer too small"
  show BadParameter = "bad parameter to WOFF function"
  show IllegalOrder = "unknown internal error"

data Warning =
    UnknownVersion
  | ChecksumMismatch
  | MisalignedTable
  | TrailingData
  | UnpaddedTable
  | RemovedDSIG
  deriving (Eq)

instance Show Warning where
  show UnknownVersion = "unrecognized sfnt version"
  show ChecksumMismatch = "checksum mismatch (corrected)"
  show MisalignedTable = "misaligned font table"
  show TrailingData = "extraneous input data discarded"
  show UnpaddedTable = "final table not correctly padded"
  show RemovedDSIG = "digital signature (DSIG) table removed"

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

wrapResult :: Ptr CUInt -> CString -> Ptr CUInt -> IO EncodeResult
wrapResult statusPtr woffData woffLenPtr = do
  status <- peek statusPtr
  let (errorCode, warnings) = statusToErrorCodeAndWarnigns status
  case errorCode of
    Ok -> do
      woffLen <- peek woffLenPtr
      woff <- BS.unsafePackMallocCStringLen (woffData, fromIntegral woffLen)
      return $ Right (woff, warnings)
    _ -> return $ Left errorCode

encode :: ByteString -> Word16 -> Word16 -> IO EncodeResult
encode sfnt major minor =
  BS.unsafeUseAsCStringLen sfnt $ \(sfntData, sfntLen) -> (
  alloca $ \woffLenPtr -> (
  alloca $ \statusPtr -> do
    woffData <- cWoffEncode sfntData (CUInt (fromIntegral sfntLen)) (CUShort major) (CUShort minor) woffLenPtr statusPtr
    wrapResult statusPtr woffData woffLenPtr))

setMetadata :: ByteString -> ByteString -> IO EncodeResult
setMetadata woff meta =
  BS.unsafeUseAsCStringLen woff $ \(woffData, woffLen) -> (
  BS.unsafeUseAsCStringLen meta $ \(metaData, metaLen) -> (
  allocaBytes woffLen $ \woffDataTmp -> (
  alloca $ \woffLenPtr -> (
  alloca $ \statusPtr -> do
    poke woffLenPtr (CUInt (fromIntegral woffLen))
    copyBytes woffDataTmp woffData woffLen
    woffData <- cWoffSetMetadata woffDataTmp woffLenPtr metaData (CUInt (fromIntegral metaLen)) statusPtr
    wrapResult statusPtr woffData woffLenPtr))))

setPrivateData :: ByteString -> ByteString -> IO EncodeResult
setPrivateData woff priv =
  BS.unsafeUseAsCStringLen woff $ \(woffData, woffLen) -> (
  BS.unsafeUseAsCStringLen priv $ \(privData, privLen) -> (
  allocaBytes woffLen $ \woffDataTmp -> (
  alloca $ \woffLenPtr -> (
  alloca $ \statusPtr -> do
    poke woffLenPtr (CUInt (fromIntegral woffLen))
    copyBytes woffDataTmp woffData woffLen
    woffData <- cWoffSetPrivateData woffDataTmp woffLenPtr privData (CUInt (fromIntegral privLen)) statusPtr
    wrapResult statusPtr woffData woffLenPtr))))

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
